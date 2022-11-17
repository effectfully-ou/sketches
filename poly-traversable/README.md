# poly-traversable

## **UPDATE**

This article is superseded by [Alternative `HasLens`](https://github.com/effectfully/sketches/tree/master/has-lens-done-right). Besides, the claims about bad type inference for the "naive" FunDep versions are wrong, see [this comment](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-568206301) by [Oleg Grenrus](https://github.com/phadej).

## `PolyTraversable`

There is the [`mono-traversable`](http://hackage.haskell.org/package/mono-traversable) package which defines monomorphic versions of the `Functor`, `Foldable` and `Traversable` classes. Monomorphic `fmap` called `omap` allows to apply a function to each element of a container and that container doesn't have to be a type constructor (i.e. a thing of kind `* -> *`).

`fmap` has this signature:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`omap` has this signature:

```haskell
omap :: (Element mono -> Element mono) -> mono -> mono
```

where `Element` is (quoted from the docs) "type family for getting the type of the elements of a monomorphic container":

```haskell
type family Element mono
type instance Element ByteString = Word8
type instance Element Text = Char
type instance Element [a] = a
type instance Element (IO a) = a
type instance Element (Maybe a) = a
...
```

This allows `omap` to cover a couple of useful cases:

1. mapping over monomorphic containers, e.g.: `omap @Text :: (Char -> Char) -> Text -> Text` (also I think there should be an `IntSet` instance which seems to be law-abiding)
2. mapping over containers that need their elements to be constrained, e.g.: `omap @(Unboxed.Vector a) :: Unbox a => (a -> a) -> Unboxed.Vector a -> Unboxed.Vector a`

But you might ask whether it would be better to allow `omap` to be polymorphic as well, so we can have

```haskell
omap @(Unboxed.Vector a) @(Unboxed.Vector b) :: (Unbox a, Unbox b) => (a -> b) -> Unboxed.Vector a -> Unboxed.Vector b
```

and 1. as before. It's easy to come up with a solution:

```haskell
class PolyFunctor s t a b
    | s -> a, t -> b     -- Elements in containers are determined by those containers.
    , s b -> t, t a -> s -- If `b` is the type of elements from `t` and `s` has the same shape as `t`,
                         -- then `b` and `s` together determine `t`. Same for the other direction.
    where
  pmap :: (a -> b) -> s -> t
```

which together with

```haskell
class PolyFoldable s a | s -> a where
  pfoldMap :: Monoid m => (a -> m) -> s m
```

naturally generalizes to

```haskell
class (PolyFunctor s t a b, PolyFoldable s a) => PolyTraversable s t a b | s -> a, t -> b, s b -> t, t a -> s where
  ptraverse :: Applicative g => (a -> g b) -> s -> g t
```

I asked myself "why is `mono-traversable` not `poly-traversable`?" and started googling. Found [this announce](https://www.yesodweb.com/blog/2013/09/classy-mono):

> For example, map was implemented through a CanMap type class:
>
>     class CanMap ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
>         map ∷ (i → o) → ci → co
>
> The previous approach of classy prelude met its design goals, but it had drawbacks. The main problem was that APIs with lots of type variables and functional dependencies give hard to decipher error messages. I also came across a case of using a complex data structure where GHC could not resolve the types and switching to the Prelude version immediately resolved it. But switching to the standard Prelude version of a function (when operating over lists) was already one of my techniques to help understand the error messages. I was much happier with the error messages from using fmap (over polymorphic containers) than using the map from classy-prelude.
>
> The original classy-prelude approach missed the chance to solve the underlying monomorphic vs polymorphic problem in a well-grounded way. This is the motivation for the mono-traversable package: we just use a type family to declare the type that the mono-morphic container contains.

Okay: bad type inference, proliferation of type variables and a lack of the single `Element` type family defined once for each type. Also that last one point causes another issue: nothing prevents us from defining multiple `PolyFunctor` instances for the same type constructor. E.g.

```haskell
instance PolyFunctor [Int] [Double] Int Double where
  pmap = fmap

instance PolyFunctor [Double] [Int] Double Int where
  pmap = fmap
```

or even

```haskell
instance PolyFunctor ByteString Text Word8 Char where
  pmap f = Text.pack . map f . BS.unpack
```

which certainly messes with inference. E.g. the following

```haskell
mapByteString :: (_ -> _) -> ByteString -> _
mapByteString = pmap
```

results in (having `PartialTypeSignatures` enabled)

```
    • Ambiguous type variables ‘w0’, ‘b0’ arising from a use of ‘pmap’
      prevents the constraint ‘(PolyFunctor
                                  ByteString w0 Word8 b0)’ from being solved.
```

while `mono-traversable` would do the right thing here and infer all the type variables.

So we want to stress that there can be only one `PolyFunctor` instance. Before doing so let's redefine `Element` in a more restrictive, but also a bit more inference-friendly way:

```haskell
type family Element c
type instance Element (f a) = a
type instance Element IntSet = Int
type instance Element ByteString = Word8
type instance Element Text = Char
```

So any thing of type `f a` is assumed to store something of type `a`. It's a pretty silly thing to state, because `a` may appear in a contravariant position, for example, and it wouldn't make sense to talk about `a` as a type variable representing some kind of elements in a data structure. But `Element` will never be used by itself, it's always associated with a particular type class which rules out the contravariant case. So you anyway won't see an error like "Couldn't match expected type `Element Predicate` with actual type `some_silly_inferred_type_full_of_random_numbers_helpfully_provided_by_the_GHC_renaming_machinery` and instead you'll see e.g. "No instance for `PolyFunctor Predicate`", hence I think type inference should take the precedence here. Though, there well might be something I haven't considered, but in any case that tiny optimization is not required for the following to work, you'd just need a few `Element (f a) ~ a` constraints without it.

The `PolyFoldable` class remains the same as the `MonoFoldable` one:

```haskell
class PolyFoldable s where
  pfoldMap :: Monoid m => (Element s -> m) -> s -> m
```

Here are the `PolyFunctor` and `PolyTraversable` classes finally:

```haskell
class s `SameShape` s => PolyFunctor s where
  pmap :: s `SameShape` t => (Element s -> Element t) -> s -> t

class (PolyFunctor s, PolyFoldable s) => PolyTraversable s where
  ptraverse :: (s `SameShape` t, Applicative g) => (Element s -> g (Element t)) -> s -> g t
```

Now `PolyFunctor` and `PolyTraversable` are single parameter type classes. `pmap` says that it can map `s` to any `t` provided `s` and `t` have the same shape (same for `PolyTraversable`). And as an instance constraint we also require that `s` is of the same shape as `s`, i.e. itself.

We need the ``s `SameShape` s`` constraint in order for these and similar things to type check:

```haskell
omap :: PolyFunctor s => (Element s -> Element s) -> s -> s
omap = pmap

pfoldMapDefault :: forall s m. (PolyTraversable s, Monoid m) => (Element s -> m) -> s -> m
pfoldMapDefault f = getConst . ptraverse @s @s (Const . f)
```

It only remains to define `SameShape`. We could write

```haskell
type family s `SameShape` t where
  f _ `SameShape` g _ = f ~ g
  s   `SameShape` t   = s ~ t
```

thus essentially saying that shapes are equal in the polymorphic case if type constructors are equal and in the monomorphic case if whole containers are equal. But here we do pattern matching on both `s` and `t` while it would be more inference-friendly to be able to infer the shape of `s` when the shape of `t` is known and vice versa. Hence:

```haskell
type family s `DeterminesShapeOf` t where
  f _ `DeterminesShapeOf` t = t ~ f (Element t)
  s   `DeterminesShapeOf` t = t ~ s

type s `SameShape` t = (s `DeterminesShapeOf` t, t `DeterminesShapeOf` s)
```

If we now add a default instance implementation to each class, e.g.:

```haskell
  default pmap :: (s ~ f a, t ~ f b, Functor f) => (Element s -> Element t) -> s -> t
  pmap = fmap
```

it becomes possible to derive instances for `Poly*` classes from their base counterparts:

```haskell
instance PolyFunctor [a]
instance PolyFunctor (Maybe a)
instance PolyFunctor (Const b a)

instance PolyFoldable [a]
instance PolyFoldable (Maybe a)
instance PolyFoldable (Const b a)

instance PolyTraversable [a]
instance PolyTraversable (Maybe a)
instance PolyTraversable (Const b a)
```

and provide instances for monomorphic containers manually:

```haskell
pfoldMapViaFoldr
  :: Monoid m
  => (forall b. (Element s -> b -> b) -> b -> s -> b)
  -> (Element s -> m) -> s -> m
pfoldMapViaFoldr fr f = fr (mappend . f) mempty

ptraverseViaPackUnpack
  :: (Applicative g, Traversable f)
  => (f (Element t) -> t)
  -> (s -> f (Element s))
  -> (Element s -> g (Element t)) -> s -> g t
ptraverseViaPackUnpack pack unpack f = fmap pack . traverse f . unpack

instance PolyFunctor IntSet where
  pmap = IntSet.map
instance PolyFunctor ByteString where
  pmap = BS.map
instance PolyFunctor Text where
  pmap = Text.map

instance PolyFoldable IntSet where
  pfoldMap = pfoldMapViaFoldr IntSet.foldr
instance PolyFoldable ByteString where
  pfoldMap = pfoldMapViaFoldr BS.foldr
instance PolyFoldable Text where
  pfoldMap = pfoldMapViaFoldr Text.foldr

instance PolyTraversable IntSet where
  ptraverse = ptraverseViaPackUnpack IntSet.fromList IntSet.toList
instance PolyTraversable ByteString where
  ptraverse = ptraverseViaPackUnpack BS.pack BS.unpack
instance PolyTraversable Text where
  ptraverse = ptraverseViaPackUnpack Text.pack Text.unpack
```

Now the previous example

```haskell
mapByteString :: (_ -> _) -> ByteString -> _
mapByteString = pmap
```

works just fine and we get only these warnings:

```
Found type wildcard ‘_’ standing for ‘Word8’
Found type wildcard ‘_’ standing for ‘Word8’
Found type wildcard ‘_’ standing for ‘ByteString’
```

i.e. everything is inferred correctly. Same for

```haskell
mapList :: (_ -> _) -> _ -> [_]
mapList = pmap
```

which specifies to

```haskell
mapList :: (w -> w1) -> [w] -> [w1]
```

## Proving properties about `SameShape`

`SameShape` is a symmetric relation, we can check it by

```haskell
withSymmetricShapes :: s `SameShape` t => Proxy (s, t) -> (t `SameShape` s => c) -> c
withSymmetricShapes _ x = x
```

which essentially says "if you need to satisfy the ``t `SameShape` s`` constraint, it suffices to know ``s `SameShape` t``.

On the other hand, `SameShape` is not reflexive:

```haskell
withReflexiveShape :: Proxy s -> (s `SameShape` s => c) -> c
withReflexiveShape _ x = x
```

results in an error that essentially says ``s `SameShape` s`` cannot be decided. But since `SameShape` is just a type-level function, we can prove properties about it. We'll use a rather well-known trick (described [here](https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html) for one example) that allows to avoid overlapping instances, but first it's needed to tweak the definition of `DeterminesShapeOf` a bit. Since in ``s `DeterminesShapeOf` t`` there is pattern matching on `s` we need to be able to explicitly dispatch on `s` somehow in our proofs in order for `DeterminesShapeOf` to choose either the polymorphic or the monomorphic clause and reduce accordingly (this is the gist of proving with dependent types). So we introduce another type family, `ShapeOf`:

```haskell
data PolyShape (f :: * -> *)
data MonoShape s

type family ShapeOf s where
  ShapeOf (f _) = PolyShape f
  ShapeOf  s    = MonoShape s
```

which allows to explicitly dispatch on the shape of a container. Now `DeterminesShapeOf` is defined over shapes rather than containers and the `(t ~)` part is abstracted out:

```haskell
type family SpecifyShapeBy s t where
  SpecifyShapeBy (PolyShape f) t = f (Element t)
  SpecifyShapeBy (MonoShape s) _ = s

type s `DeterminesShapeOf` t = t ~ SpecifyShapeBy (ShapeOf s) t
type s `SameShape` t = (s `DeterminesShapeOf` t, t `DeterminesShapeOf` s)
```

But otherwise the definition is the same. And here is where the avoid-overlapping-instances trick comes in:

```haskell
class ShapeOf s ~ ss => KnownShapeDispatch s ss where
  withKnownShape :: proxy s -> (forall f a. s ~ f a => c) -> (ShapeOf s ~ MonoShape s => c) -> c

instance s ~ f a => KnownShapeDispatch s (PolyShape f) where
  withKnownShape _ poly _ = poly

instance (ShapeOf s ~ MonoShape s, s ~ s') => KnownShapeDispatch s (MonoShape s') where
  withKnownShape _ _ mono = mono

type KnownShape s = KnownShapeDispatch s (ShapeOf s)
```

The type signature of the `withKnownShape` function says that whenever something holds in the polymorphic and in the monomorphic cases, it just holds. We explicitly dispatch on the shapes in the instances and choose appropriate arguments of the `withKnownShape` function. Now reflexivity is provable:

```haskell
withReflexiveShape :: KnownShape s => Proxy s -> (s `SameShape` s => c) -> c
withReflexiveShape sProxy x = withKnownShape sProxy x x
```

This says that since it is obvious that ``s `SameShape` s`` holds for both polymorphic and monomorphic `s`, it holds for any `s`.

Transitivity is derivable as well:

```haskell
withTransitiveShapes
  :: forall s t u c. (KnownShape s, s `SameShape` t, t `SameShape` u)
  => Proxy (s, t, u) -> (s `SameShape` u => c) -> c
withTransitiveShapes _ x = withKnownShape (Proxy :: Proxy s) x x
```

## Polymorphic record update

Another use case for such a representation is [polymorphic record update](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Design#Limitedtype-changingupdate). Quoting the wiki:

```haskell
type family UpdTy (r :: *) (n :: Symbol) (a :: *) :: *

class (Has r n (FldTy r n), r ~ UpdTy r n (FldTy r n))
   => Upd (r :: *) (n :: Symbol) (t :: *) where
  setField :: Proxy# n -> r -> t -> UpdTy r n t
```

That has unidirectional type inference and with the technique described above we can make it bidirectional. Also this should solve the phantom arguments problem described in the wiki, but I didn't check. I guess we'll need to move `DeterminesShapeOf` to the `Upd` type class itself.
