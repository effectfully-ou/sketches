# Enumerating type variables

## Preface

In the [previous post](https://github.com/effectfully/sketches/tree/master/poly-type-of-saga/part1-try-unify) we learned how to automatically detect and instantiate polymorphism. In this post we'll see how to use that trick to enumerate type variables in a polymorphic type and reify it at the term level.

How do we reify any type-level information at the term level? Via a type class. For example, having this class:

```haskell
class TypeToString a where
    typeToString :: Proxy a -> String
```

(if you don't know what that `Proxy` thing is, check [this post](https://kseo.github.io/posts/2017-01-15-data-proxy.html) out)

and a bunch of instances for it:

```haskell
instance TypeToString Bool where
    typeToString _ = "Bool"

instance TypeToString Int where
    typeToString _ = "Int"

instance (TypeToString a, TypeToString b) => TypeToString (a, b) where
    typeToString _ = concat ["(", typeToString (Proxy @a), ", ", typeToString (Proxy @b), ")"]

instance TypeToString a => TypeToString [a] where
    typeToString _ = concat ["[", typeToString (Proxy @a), "]"]
```

we can get the `String` representation of a type:

```
>>> typeToString $ Proxy @[(Int, Bool)]
"[(Int, Bool)]"
```

`TypeToString` works only for those types that we've explicitly provided instances for. All of which have to be monomorphic. Our task is to generalize the idea to work for types with arbitrary mononomorhic types in them (without providing an instance for each such type), as well as arbitrary amount of type variables of arbitrary kinds.

Let's start with talking about what to instantiate type variables with.

## What to instantiate variables with?

Consider a type like `(a -> b) -> f a -> f b`. It has three type variables: two of kind `*` (`a` and `b`) and one of kind `* -> *` (`f`). How do we instantiate those the variables to be able to match on them? We could define a bunch of types like

```haskell
data Var (i :: Nat)
data ApplyVar (i :: Nat) (a :: *)
data ApplyApplyVar (i :: Nat) (a :: *) (b :: *)
<...>
```

where each type carries the id of the type variable (so that we can distinguish between different type variables) and receives as many types as it needs to fit into the right kind. Then we monomorphize the original type using the following instantiations:

```
a ~ Var 0
b ~ Var 1
f ~ ApplyVar 2
```

and get this specialized type:

```
(Var 0 -> Var 1) -> ApplyVar 2 (Var 0) -> ApplyVar 2 (Var 1)
```

Which is something that we could later match on using a class.

However we'd then need a `Var`-like data type for each possible kind, including higher ones like `(* -> *) -> *`, which is not only an infinite number of types, but also quite a lot even if we restrict ourselves to some "practical" subset of all possible kinds. And in order to reify such types, we'd need to provide an instance for each of them. That's quite a lot of boilerplate, it would be much better if we could somehow handle the general case without needing to define a data type per kind.

Luckily, GHC is a big weirdo and allows us to inhabit any kind. Like, _any_. Tired of choosing only between `True` and `False` when creating a `Bool`? Bring new faces to the party!

```haskell
type family WhoKnows :: Bool where { }

data Conjecture
    = Euler
    | Poincare
    | Collatz

type Holds :: Conjecture -> Bool
type family Holds b where
    Holds 'Euler    = 'False
    Holds 'Poincare = 'True
    Holds 'Collatz  = WhoKnows
```

However `WhoKnows`, being a type family, is unmatchable and so we can match on it neither in a type family nor in a type class. What is it that is like a `type`, but matchable? `data`!

```haskell
data family NotSure :: bool
```

GHC doesn't like it if we specify the type of `NotSure` as `Bool`, so we make it something that can be specialized to `Bool`. Weird inconsistency. Anyway, now we can implement the `Or` operator from a three-valued logic and pretend it works over `Bool`s:

```haskell
type Or :: Bool -> Bool -> Bool
type family a `Or` b where
    'False  `Or` 'False  = 'False
    'True   `Or` _       = 'True
    _       `Or` 'True   = 'True
    _       `Or` _       = NotSure
```

So this is how our general-case `Var` looks like:

```haskell
type Var :: forall k. Nat -> k
data family Var i
```

It allows us to instantiate

```
(a -> b) -> f a -> f b
```

to

```
(Var 0 -> Var 1) -> Var 2 (Var 0) -> Var 2 (Var 1)
```

which is nice and easy to deal with.

## Plan

Reification is going to comprise three phases (we could do everything in a single phase, but that would look really messy):

1. traverse the type and instantiate every type variable with `Var i` for some `i`
2. directly reify the now fully monomorphic type at the term level using a type class similar to `TypeToString`
3. convert the directly reified type to a final representation (by replacing all ids with actual variable names etc)

Full code is available in [src/Main.hs](src/Main.hs).

## The first phase

The first phase is very similar to what we did with `DefaultAllTo` in the previous post: just like with `DefaultAllTo` we'll need to deconstruct the type into a function and its argument and recurse into the pieces until no deconstruction is possible, at which point we've got a leaf, which we try to instantiate as `Var i` for a fresh `i`. That "fresh `i`" part suggests that we need some kind of type-level state to thread through instance invocations. The usual way of handling type-level state is via [indexed monads](https://kseo.github.io/posts/2017-01-12-indexed-monads.html) and we'll use the same idea except with indexed monads one parameterizes various `data` by indices and we'll parameterize a `class`:

```haskell
type EnumerateFromTo :: forall b. Nat -> Nat -> b -> Constraint
class EnumerateFromTo i j y | i y -> j
```

`EnumerateFromTo` receives:

1. `i` -- the initial fresh id for another uninstantiated type variable
2. `j` -- the final fresh id after all type variables in a type are instantiated
3. `y` -- the type itself (of arbitrary kind `b`)

Clearly, the final fresh id depends on the initial one and the type being processed, hence the functional dependency.

An instance for deconstruct-and-recurse is straightforward:

```haskell
instance
    ( EnumerateFromTo i j f
    , EnumerateFromTo j k x
    ) => EnumerateFromTo i k (f x)
```

Note how enumeration of type variables for the argument starts with the same fresh id (`j`) that enumeration for the function ended with.

The base case is a bit more interesting:

```haskell
instance {-# INCOHERENT #-}
    ( x ~ Var @b i                     -- [1]
    , x ~?~ y                          -- [2]
    , j ~ If (x === y) (i + 1) i       -- [3]
    ) => EnumerateFromTo i j (y :: b)
```

Line by line:

1. Assign `Var @b i` (which is the same thing as `Var i :: b`, but uses a type-level type application for brevity) to `x`.
2. Try to unify `x` (the freshly created variable) with `y` (the leaf, possibly an uninstantiated type variable, which then gets instantiated to `x`. But maybe something else, in which case nothing happens).
3. If unification succeeds, then `x === y` is `'True` and so we used the fresh id that we had and now need to increase the counter. Otherwise the new counter (`j`) is the same as the old one (`i`).

And that's all we need for the first phase (but there's a catch).

## The second phase

For the second phase we need a term-level representation of types. This one is good enough:

```haskell
data VarInfo = VarInfo
    { _varInfoId   :: Integer              -- ^ The id of the variable.
    , _varInfoKind :: ReifiedType VarInfo  -- ^ The kind of a type variable. Which is in fact a type.
                                           -- I.e. we pretend we have `TypeInType`.
    } deriving (Eq)

-- The only reason why I parameterize 'ReifiedType' by @var@ is to get folding over variables for free.
data ReifiedType var
    = RVar var                                    -- ^ A type variable.
    | RApply (ReifiedType var) (ReifiedType var)  -- ^ A type-level application.
    | RArrow                                      -- ^ @(->)@
    | RStar                                       -- ^ @*@
    | RMeta String                                -- ^ A meta type (like 'Int' or '[]').
    deriving (Eq, Foldable)
```

Reification then is handled via a type class similar to `TypeToString` from the above:

```haskell
class ReifyType (a :: k) where
    reifyType :: Proxy a -> ReifiedType VarInfo
```

Here are a couple of trivial instances:

```haskell
instance ReifyType (*) where
    reifyType _ = RStar

instance ReifyType (->) where
    reifyType _ = RArrow
```

Handling variables is also straightforward, we just need to demote the natural representing the id of the variable and reify the kind of the variable:

```haskell
instance {-# OVERLAPPING #-} (KnownNat i, ReifyType k) => ReifyType (Var i :: k) where
    reifyType _ = RVar $ VarInfo (natVal @i Proxy) (reifyType $ Proxy @k)
```

Any other type-level application (note that `Var i` is a type-level application itself) is handled by reifying the function and the argument separately:

```haskell
instance {-# OVERLAPS #-} (ReifyType f, ReifyType a) => ReifyType (f a) where
    reifyType _ = RApply (reifyType $ Proxy @f) (reifyType $ Proxy @a)
```

And the base case (i.e. when a type is not a type-level application) is

```haskell
instance {-# OVERLAPPABLE #-} Typeable any => ReifyType any where
    reifyType = RMeta . show . typeRepTyCon . typeRep
```

Here we defer to `Data.Typeable` to provide us the textual representation of the type (kudos to [Kana](https://github.com/kana-sama) for the idea). GHC automatically provides a `Typeable` instance for a data type, so we don't even need to derive anything and reification of monomorphic types just works.

And that's it, no more instances are needed. It's possible to avoid using overlapping instances [like this](https://github.com/effectfully/sketches/tree/master/avoid-overlapping-recursive), but that would complicate the code without bringing any substantial benefits beyond enabling me to brag about how I avoided using overlapping instances.

## The third phase

For the third phase we need a final term-level representation of types. Here it is:

```haskell
data PolyType
    = Var String
    | PolyType :$ PolyType
    | Arrow
    | Star
    | Meta String
    | Forall [(String, PolyType)] PolyType
```

It's the same as `ReifiedType` except we use actual names for variables and there's an additional constructor: `Forall`. It binds a list of type variables (each of which has a name and a kind).

I'm not going to describe how we convert from one term-level representation of types to another, it's pretty mundane stuff, so just check source code if you're interested.

## Testing

Our final definition is this:

```haskell
polyTypeOf :: forall a k. (ReifyType a, EnumerateFromTo 0 k a) => a -> PolyType
polyTypeOf _ = toPolyType . reifyType $ Proxy @a
```

We can check that it does the right thing for monomorphic types:

```
>>> polyTypeOf True
Bool
>>> polyTypeOf "I'm a string"
[Char]
```

As well as for polymorphic ones:

```
>>> polyTypeOf $ \x y -> x
forall (a :: *) (b :: *). a -> b -> a
>>> polyTypeOf ($)
forall (a :: *) (b :: *). (a -> b) -> a -> b
>>> polyTypeOf $ let fix f = f (fix f) in fix
forall (a :: *). (a -> a) -> a
>>> polyTypeOf map
forall (a :: *) (b :: *). (a -> b) -> [a] -> [b]
```

But if we attempt to retrieve the type of a polymorphic binding with constraints in it, we'll get this:

```
>>> polyTypeOf 3
<interactive>:591:13: error:
    • No instance for (Num (Var 0)) arising from the literal ‘3’
    • In the first argument of ‘polyTypeOf’, namely ‘3’
      In the expression: polyTypeOf 3
      In an equation for ‘it’: it = polyTypeOf 3
```

Whoops. For the time being we can just ignore the problem by providing a bunch of fake instances:

```haskell
instance Eq (Var i)
instance Num (Var i)
instance Foldable (Var i)
instance Bifunctor (Var i)
```

And now it works (but the constraints are dropped):

```
>>> polyTypeOf 3
forall (a :: *). a
>>> polyTypeOf elem
forall (a :: *) (f :: * -> *). a -> f a -> Bool
>>> polyTypeOf bimap
forall (a :: *) (b :: *) (c :: *) (d :: *) (f :: * -> * -> *). (a -> b) -> (c -> d) -> f a c -> f b d
```

We can do better here and not drop the constraints, but that's for another blog post.

Finally, recall I said there was a catch. Here it is:

```
>>> polyTypeOf Proxy
<interactive>:576:2-17: error:
    • No instance for (Typeable k0) arising from a use of ‘polyTypeOf’
    • In the expression: polyTypeOf Proxy
      In an equation for ‘it’: it = polyTypeOf Proxy
```

We didn't get dependent kinding right! Well, this is no suprise given how we defined the base case for `EnumerateFromTo`:

```haskell
instance {-# INCOHERENT #-}
    ( x ~ Var @b i
    , x ~?~ y
    , j ~ If (x === y) (i + 1) i
    ) => EnumerateFromTo i j (y :: b)
```

The kind (`b`) of the leaf (`y`) may contain uninstantiated variables itself, but we do not account for that. We solved this exact problem	for `DefaultAllTo` in the previous post, but solving it for `EnumerateFromTo` is a remarkable pain in the ass, so this is also for another blog post.

## Conclusions

So here you have it, enumeration of type variables. Not too hacky, but that's only because we handle neither dependent kinding nor constraints right.

I'll probably do a post on handling constraints next time.

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
