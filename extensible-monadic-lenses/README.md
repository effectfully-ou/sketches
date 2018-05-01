# extensible-monadic-lenses

## Preface

In the [previous](https://github.com/effectfully/sketches/tree/master/ann-monad-var) post I described a problem that arises when you have large nested state full of distinct variables and a solution to the problem: monadic setters which work across various monadic stacks and types of variables. But that's not quite satisfactory, because with this solution each write requires a redundant read and because we have just a setter while a proper lens is desirable. Why is the former the case? This is due to how `Lens` is defined:

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

Here that thing of type `a -> f b` is a function that receives the value of a field (in a record, tuple or whatever) and transforms it in some way. On writing, the original value is simply ignored in the pure case. But in the monadic case "ignored" means "read and then thrown away". So each write requires a read. How to solve this? Do not define `Lens` as something that receives a function, instead generalize so it can receive an action to perform encoded as a regular Haskell data type. Is there a known `Lens` representation which allows that? Yes, [profunctor optics](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf):

```haskell
type Optic i o s t a b = i a b -> o s t
```

Here `i` is an input profunctor, `o` is an output profunctor and other indices are as before. This representation is a bit generalized, since in the usual one `i` and `o` are the same. `Lens` is defined in the usual way:

```haskell
type Lens s t a b = forall p. Strong p => Optic p p s t a b
```

## Initial

Let's now consider things we're able to do in this setting.

The type of actions which can be performed over a variable:

```haskell
data ActionResult
  = Unit
  | Input

data Action r f a b where
  Read    ::               Action 'Input f a b
  Write   ::  b         -> Action 'Unit  f a b
  Mutate  :: (a -> b)   -> Action 'Unit  f a b
  MutateM :: (a -> f b) -> Action 'Unit  f a b

type family GetActionResult d a where
  GetActionResult 'Unit  a = ()
  GetActionResult 'Input a = a
```

`ActionResult` and `GetActionResult` specify the resulting type of an action. On `Read` `a` is returned, on other operations the empty tuple is returned.

And the main definition is

```haskell
_Var :: (MonadRead m v, MonadMutateM_ f m v) => Optic (Action r f) (->) (v a) (m (GetActionResult r a)) a a
_Var  Read       var = Var.read     var
_Var (Write   x) var = Var.write    var x
_Var (Mutate  f) var = Var.mutate_  var f
_Var (MutateM f) var = Var.mutateM_ var f
```

which turns the code for an action into the actual action. Note that the `_Var` lens is built on top of the `monad-var` classes, so the concepts are nicely separated.

In the `Read` case the type signature boils down to `Action 'Input f a a -> v a -> m a`.

In the `Write` case type signature boils down to `Action 'Unit f a a -> v a -> m ()`. The `Mutate` and `MutateM` cases are identical to the `Write` case.

Having this setup we can define setters that work both for the pure and the `Action` cases:

```haskell
class Writeable i where
  (.~) :: Optic i o s t a b -> b -> o s t

instance Writeable (->) where
  _Lens .~ y = _Lens (const y)

instance r ~ 'Unit => Writeable (Action r f) where
  _Lens .~ y = _Lens (Write y)
```

An input profunctor of an `Optic` determines which implementation of `(.~)` to choose.

If the input profunctor is `(->)`, then the type signature of `(.~)` reduces to `((a -> b) -> o s t) -> b -> o s t)` which has this interpretation: whenever an `Optic` receives a function that modifies some field, you can ignore the original value of the field and just put there another value.

If the input profunctor is `Action r f`, then the type signature of `(.~)` reduces to `(Action r f -> o s t) -> b -> o s t)` which means that we can supply any `Action` to an `Optic` including the desired `Write` applied to a value of type `b`.

Now not only the usual pure case works:

```haskell
('a', 'b') & _1 .~ 'c'
```

but also monadic ones:

```haskell
do
  var <- newIORef 'a' -- Create a new variable.
  var & _Var .~ 'b'   -- Set the variable (uses only `writeIORef` internally).
```

`(%~)` can be defined similarly to `(.~)`.

But `Action` is closed meaning we can't extend it with new operations. An extensible verison goes next.

## Extensible

We do not touch `Writeable` and its instance for `(->)`. Various monadic extensions shouldn't affect common pure cases. To make actions extensible we split `Action` into several data types. Let's consider the `Write` and `Mutate` cases:

```haskell
data WriteAction r a b where
  WriteAction :: b -> WriteAction () a b

data MutateAction r a b where
  MutateAction :: (a -> b) -> MutateAction () a b

instance r ~ () => Writeable (WriteAction r) where
  _Lens .~ y = _Lens $ WriteAction y

instance r ~ () => Mutateable (MutateAction r) where
  _Lens %~ f = _Lens $ MutateAction f

class Variable action m v where
  _Var :: Optic (action r) (->) (v a) (m r) a a

instance MonadWrite m v => Variable WriteAction m v where
  _Var (WriteAction y) var = Var.write var y

instance MonadMutate_ m v => Variable MutateAction m v where
  _Var (MutateAction f) var = Var.mutate_ var f
```

Here instead of a single `Action` we have `WriteAction` and `MutateAction`. `_Var` is a method of the `Variable` class instead of a simple function. The definition of `_Var` is basically the same as before except it's now split across several instances rather than being defined by case analysis.

Is this a solution? Not quite. Consider a previous example: `var & _Var .~ 'b'`. It doesn't type check, because now both `_Var` and `(.~)` are methods of type classes and neither of the provided instances connects one class to the another. Type inference simply doesn't work in this situation, so we have to write things like `var & _Var @WriteAction .~ 'b'`. The problem comes from the fact that `(.~)` over a `Variable` doesn't specify that the only sensible action to perform is `WriteAction`. Let's fix this.

## Inferrable

```haskell
newtype Action action r a b = Action (action r a b)

data WriteAction r a b where
  WriteAction :: b -> WriteAction () a b

data MutateAction r a b where
  MutateAction :: (a -> b) -> MutateAction () a b

instance (action ~ WriteAction, r ~ ()) => Writeable (Action action r) where
  _Lens .~ y = _Lens $ Action (WriteAction y)

instance (action ~ MutateAction, r ~ ()) => Mutateable (Action action r) where
  _Lens %~ f = _Lens $ Action (MutateAction f)

class Variable action m v where
  _Var :: Optic (Action action r) (->) (v a) (m r) a a

instance MonadWrite m v => Variable WriteAction m v where
  _Var (Action (WriteAction y)) var = Var.write var y

instance MonadMutate_ m v => Variable MutateAction m v where
  _Var (Action (MutateAction f)) var = Var.mutate_ var f
```

`Action` is a `newtype` wrapper around any supplied `action`. The `action ~ WriteAction` constraint in the `Writeable (Action action r)` instance explicitly says that the only permitted action to be used with `(.~)` is `WriteAction`. `Action` also has been added to the type of `_Var` which is how the two distinct notions of `(.~)` and `Variable` become connected. So when you write something like `var & _Var .~ 'a'`, the instance resolution algorithm determines that since `_Var` has the `Action action r` input profunctor, the only possible `action` is `WriteAction`.

Now we can use lenses in the usual pure way and also monadically write/mutate variables. But we also want to be able to compose monadic lenses with pure ones.

## Composable

What does a thing like `var & _Var . _1 .~ 'a'` mean? It's writing to a variable, but this writing does require a read, because we overwrite only the first element of a tuple stored in the variable and the second must be kept the same. Hence `WriteAction` gets transformed into `MutateAction` sometimes. This is expressed as

```haskell
newtype Action action a b = Action (action a b)

-- An alternative definition of the `Variable` type class. Seems to be more convenient and general.
class Variable action m v r | action -> r where
  _Var :: Optic (Action action) (->) (v a) (m r) a a

data AlterAction a b
  = WriteAction b
  | MutateAction (a -> b)

instance MonadMutate_ m v => Variable AlterAction m v () where
  _Var (Action (WriteAction  y)) var = Var.write   var y
  _Var (Action (MutateAction f)) var = Var.mutate_ var f

instance Profunctor AlterAction where
  dimap _ g (WriteAction  y) = WriteAction  $ g y
  dimap f h (MutateAction g) = MutateAction $ h . g . f

instance Strong AlterAction where
  first'  (WriteAction  y) = MutateAction $ \(_, z) -> (y, z)
  first'  (MutateAction f) = MutateAction $ \(x, z) -> (f x, z)

  second' (WriteAction  y) = MutateAction $ \(z, _) -> (z, y)
  second' (MutateAction f) = MutateAction $ \(z, x) -> (z, f x)
```

`WriteAction` and `MutateAction` are constructors of the same data type and in the last instance `WriteAction` turns into `MutateAction` -- this makes `var & _Var . _1 .~ 'a'` work perfectly.

## Reading

We've considered only writing and mutating so far, can reading be generalized to work both in the pure and effectful case? Yes:

```haskell
class Readable i a | i -> a where
  readable :: Optic i o s t a b -> o s t

instance Readable (Forget a) a where
  readable _Lens = _Lens $ Forget id

data ReadAction r a b where
  ReadAction :: ReadAction a a b

instance action ~ ReadAction a => Readable (Action action) a where
  readable _Lens = _Lens $ Action ReadAction

-- Pure.
(^.) :: s -> Optic (Forget a) (Forget a) s t a b -> a
a ^. _Lens = runForget (readable _Lens) a

-- Effectful.
(^!) :: Readable i a => s -> Optic i (->) s t a b -> t
(^!) = flip readable

instance MonadRead m v => Variable (ReadAction a) m v a where
  _Var (Action ReadAction) var = Var.read var
```

This allows to write `var ^! _Var` (which reduces to `Var.read var`).

## Another example

The `_File` lens:

```haskell
forceFoldable :: Foldable f => f a -> f a
forceFoldable xs = foldl' (flip seq) () xs `seq` xs

newtype WeakAction action a b = WeakAction (action a b)

newtype WeakWriteAction a b = WeakWriteAction b

newtype ModifyAction a b = ModifyAction (a -> b)

deriving instance Profunctor action => Profunctor (WeakAction action)

instance Profunctor WeakWriteAction where
  dimap _ g (WeakWriteAction y) = WeakWriteAction $ g y

deriving instance Profunctor ModifyAction

instance action ~ ReadAction a => Readable (WeakAction action) a where
  readable _Lens = _Lens $ WeakAction ReadAction

instance (action ~ WeakWriteAction) => Writeable (WeakAction action) where
  _Lens .~ y = _Lens $ WeakAction (WeakWriteAction y)

instance (action ~ ModifyAction) => Mutateable (WeakAction action) where
  _Lens %~ f = _Lens $ WeakAction (ModifyAction f)

class File action r | action -> r where
  _File :: Optic (WeakAction action) (->) String (IO r) String String

instance File (ReadAction a) a where
  _File (WeakAction ReadAction) file = readFile file

instance File WeakWriteAction () where
  _File (WeakAction (WeakWriteAction s)) file = writeFile file s

instance File ModifyAction () where
  _File (WeakAction (ModifyAction f)) file =
    readFile file >>= evaluate . forceFoldable . f >>= writeFile file
```

`_File` can be used in order to read from files, write to files and modify files. E.g.

```haskell
do
  "stuff.txt" & _File .~ "stuff"
  "stuff.txt" ^! _File >>= putStrLn
```

will create a file "stuff.txt", write the "stuff" string to it, then read that file and print its contents. Here we define another `newtype` for actions: `WeakAction`. The name is due to `WeakWriteAction` being not, well, `Strong`, so we forbid things like `file & _File . _Head .~ 'a'` thus communicating the fact that it's not possible to modify a file partly (`ModifyAction` is `Strong`, but this instance is not added for the same reason).

## Zooming

I started to think about all of this, because I had a huge state full of different variables and I wanted to modify them without boilerplate. Some variables were packaged together into records, so a way to zoom into that state was also required. Here's a simple operator:

```haskell
_Of :: (s -> a) -> Lens s b a b
_Of = lmap

-- | Go down by the first lens into a data structure and apply the second lens to the result.
-- This throws away the part of the structure skipped by the first lens.
(./)
  :: Strong o
  => Optic (Forget s) (Forget s) v x s y
  -> Optic  i          o         s t a b
  -> Optic  i          o         v t a b
_G ./ _U = _Of (^. _G) . _U
```

E.g. `('a', ('b', 'c')) & _2 ./ _2 %~ succ` results in `('b','d')` and `(./)` supports monadic lenses as well: `('d', var) & _2 ./ _Var %~ succ` is essentially the same as `Var.mutate_ var succ`.

It's also possible to zoom into a state monadically:

```haskell
(!/)
  :: (Readable i s, Monad m)
  => Optic i (->) u (m v) s y
  -> Optic j (->) v (m t) a b
  -> Optic j (->) u (m t) a b
_G !/ _U = \u -> readable _G >=> _U u
```

An example of use:

```haskell
do
  fileVar <- newIORef "stuff.txt"
  -- Replicate each letter twice in a file which name is written in a file
  -- which name is stored in the `fileVar` variable.
  fileVar & _Var !/ _File !/ _File %~ concatMap (replicate 2)
  -- Read and print the result.
  fileVar ^! _Var !/ _File !/ _File >>= putStrLn
```

Hence "zooming" works in both the pure and effectful cases and plays well with getters and setters.

## Conclusions

We've considered a framework which allows to define extensible monadic lenses that compose well (together and with pure ones). Not only lenses themselves are extensible, but also classes of actions and actions in those classes are extensible as well. Pure and effectful setters share the same operator names. And this framework is built on top of a somewhat well-known encoding of lenses, so the whole profunctor optics infrastructure is at our hands. Another very pleasant thing is that the described representation of lenses mixed with [indexed profunctor optics](http://oleg.fi/gists/posts/2017-04-26-indexed-poptics.html) finally gives us a two-digit number of type variables:

```haskell
type Optic p q i j k l s t a b = p i j a b -> q k l s t
```

which I think is enough in order to get back into the ivory tower.

The code can be found in the `src/Main.hs` file.
