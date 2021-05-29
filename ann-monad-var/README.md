## The problem

It's a common practice in Haskell to separate pure code from effectful as much as possible. One instance of said practice is to store the state of your program in some huge record, define pure updaters of this state and apply them in a `State` monad or put the state into some kind of variable (`IORef`, `MVar`, `TVar`, whatever) and update the contents of the variable. But there is a problem with this approach: suddenly, just to update a field you need to unpack and pack the whole state. Once you're in such a position, `Lens` (or similar libraries) is the only sensible way to make updates, because otherwise it's an enormous amount of boilerplate, but this causes even more computational overhead. In

```haskell
st & x .~ ...
   & y .~ ...
   & z .~ ...
```
each `.~` assings a value to some field, but also performs the unpacking and packing. This constantly bothered me while I was writing a soft real-time application (though, I've never measured the overhead). GHC RTS is optimized for throughput, but it's still better to generate less garbage. Then I realized that different parts of my state have different access requirements: some fields were accessed only from a single thread, some needed locks, some were used in `STM` transactions. So I decided to split that huge state (still assembling everything together in a single environment) across different types of variables and records of variables some of which stored records with various related data. This made the code look way more structured and fine-grained and now it was clear which parts are related and how they are accessed, but two more problems arose:

 - common variable types in Haskell have distinct interfaces. E.g. there is the `writeIORef` function, but no predefined `writeMVar` exists. What's worse is that sometimes you need functions like "modify a variable and return its old value" and "modify a variable and return its new value along with some additional value" and you have to redefine this stuff each time you switch from one variable type to another (and I also had custom combinators like "apply `succ` to a variable and if it's `0`, then apply `succ` again").
 - `Lens` no longer helps, basically. E.g. having the snippet below how would you define a modifier for an `A` value stored deep inside `D`?

```haskell
data A
  = A1
  | A2

data B = B
  { _bA :: A
  }

data C
  = C

data BCVars = BCVars
  { _bVar :: IORef B
  , _cVar :: IORef C
  }

data D = D
  { _bcVars :: BCVars
  }
```

## A solution

So I wrote a library that deals with these problems: `monad-var` ([github](https://github.com/effectfully/monad-var), [hackage](https://hackage.haskell.org/package/monad-var)). There are classes like

```haskell
class Monad m => MonadRead m v where
  read :: v a -> m a
```

and

```haskell
class (MonadMutate m v, MonadMutateM_ f m v) => MonadMutateM f m v where
  -- | Monadically mutate a variable and return an additional value.
  mutateM :: v a -> (a -> f (a, b)) -> m b
```

and combinators like

```haskell
-- | Monadically mutate a variable and also return its new value.
preMutateM_
  :: (MonadMutateM f m v, Functor f) => v a -> (a -> f a) -> m a
preMutateM_ v f = mutateM v $ \x -> f x <&> \y -> (y, y)
{-# INLINE preMutateM_ #-}
```

It also has some lenses for variables, e.g. the task from the above can be solved as

```haskell
d & bcVars . bVar ./ _Var . bA .~ A2
```

Besides, it is possible to modify a variable monadically, an example taken from the documentation:

```haskell
do
  v <- newIORef a
  v & _VarM %~ a -> succ a <$ putStr (show a)
  readIORef v >>= print
```

which prints

```
'a''b'
```

The library makes some opinionated choices. E.g. there is that `read` function (from the `MonadRead` type class) which has the name already reserved in the `Prelude`. I regret each time I use this function instead of `readMaybe`, so I always hide it in my custom preludes and there are no clashes for me. Your mileage may vary of course, but in any case you can import `MonadVar` `qualified` as `Var` and then write `Var.read`.

All `mutate*` functions are strict.

Type inference is top-down. I.e. in

```haskell
instance IO ~ io => MonadMutateM  io IO IORef where
  -- mutateM :: IORef a -> (a -> io (a, b)) -> IO b
  mutateM = ...
```

we rely on the fact that the outer context is known to be `IO` and infer that the inner context is `IO` as well. It could be

```haskell
instance IO ~ io => MonadMutateM  IO io IORef where
  -- mutateM :: IORef a -> (a -> IO (a, b)) -> io b
  mutateM = ...
```

which can be read as "if the inner context is known to be `IO` (most likely by type inference), then infer the outer context to be `IO` as well". But I consider this style of type inference a bad practice and a historical accident which goes back to days when write-no-type-signatures-get-type-checking-for-free was one of the selling points of Hindley-Milner type systems and we're currently far beyond those. With the plethora of advanced type features we now have it's better to ditch Hindley-Milner type inference with all its fragility altogether and just use top-down type inference (or better something as sophisticated as higher-order dynamic pattern unification) everywhere. When I started to write more Haskell and less Agda, I was quite surprised by the fact that Agda, despite all the remarks about undecidable type inference, has way more handy type inference than Haskell. E.g.

```haskell
silly :: Bool
silly = s where
  s = _
```

results in

```
    • Found hole: _ :: t
      Where: ‘t’ is a rigid type variable bound by
               the inferred type of s :: t at prog.hs:3:3
    • In the expression: _
```

while it clearly should be `Found hole: _ :: Bool` and Agda would do the right thing here.

Previously there were rather controversial `MonadTrans` instances:

```haskell
instance (MonadTrans t, Monad (t m), MonadWrite m v) => MonadWrite (t m) v where
  write = lift .* write
  {-# INLINE write #-}

instance (MonadTrans t, Monad (t m), MonadMutate  m v) => MonadMutate  (t m) v where
  mutate = lift .* mutate
  {-# INLINE mutate #-}
```

which were very convenient for me as I got an immediate and huge advantage: all defined setters worked across different monadic stacks and base monads. Most of the time I needed to modify a `TVar` in `IO` or `t IO`, but sometimes in `STM`, defining all these modifiers manually or placing `liftIO . atomically` everywhere or zooming into a data structure in-place was quite annoying and noisy.

I removed those instance since it's better for a public library to be more flexible.

## Problems

But lenses in `monad-var` are not quite satifactory. Here are the problems:

 - `_Var` and `VarM` are setters, so you have to define getters separately.
 - `_Var` and `_VarM` are distinct combinators and you can't just have one setter for both pure and monadic mutating. This means that in order to define getters and pure and monadic setters for some fields, essentially 3x code duplication is required.
 - How are we able to both write and modify a variable using a single setter? That's because we always read the contents of a variable before writing anything to it, so even though `v & _Var .~ x` doesn't need to call `read`, it calls and ignores the result.

And there is an encoding that seems to solve all these problems. It uses profunctor optics, so you'll like it. Stay tuned.

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
