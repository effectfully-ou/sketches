# Automatically detecting and instantiating polymorphism

## Preface

[`Data.Typeable`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Typeable.html) allows the programmer to get their on hands on a textual representation of the type of a monomorphic expression (among other things). Here's a simple example (assuming `Data.Typeable` is imported):

```
>>> typeOf 'a'
Char
```

`typeOf` also works with functions:

```
>>> typeOf (&&)
Bool -> Bool -> Bool
```

But an attempt to get the type of a polymorphic expression results in a type error:

```
>>> typeOf id
<interactive>:1734:2-10: error:
    • No instance for (Typeable a0) arising from a use of ‘typeOf’
    • In the expression: typeOf id
      In an equation for ‘it’: it = typeOf id
```

Wouldn't it be nice to have a `polyTypeOf` function supporting polymorphism:

```
>>> polyTypeOf id
forall (a :: *). a -> a
```

? Including polymorphic data types:

```
>>> polyTypeOf (!!)
forall (a :: *). [a] -> Int -> a
```

and type classes:

```
>>> polyTypeOf $ \x -> x == 42
forall (a :: *). (Eq a, Num a) => a -> Bool
```

and arbitrary number of type variables of arbitrary kinds:

```
>>> polyTypeOf traverse
forall (a :: *) (f :: * -> *) (b :: *) (t :: * -> *). (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

and while we're here, dependent kinding:

```
>>> polyTypeOf Proxy
forall (a :: *) (x :: a). Proxy x
```

This is a first article in a series of articles elaborating on how to build such `polyTypeOf` (all GHCi sessions above are actual ones). This is going to be a long journey, but first things first:

**DISCLAIMER**: what you're about to witness is an exceedingly egregious misuse of a type system. You may learn a few useful tricks in this and subsequent posts, but overall this whole endeavour is largely just an exercise in type gymnastics that went too far.

## `TryUnify`

Code is available in [src/Main.hs](src/Main.hs). Tested only with GHC-8.10.2.

The core concept that makes it possible to [programmatically](https://stackoverflow.com/a/535520/3237465) detect and instantiate type variables is the one of the `TryUnify` type family:

```haskell
infix 4 ===
type (===) :: forall a b. a -> b -> Bool
type family x === y where                                                 -- [1]
    x === x = 'True
    x === y = 'False

type TryUnify :: forall a b. Bool -> a -> b -> Constraint
class same ~ (x === y) => TryUnify same x y                               -- [2]
instance (x === y) ~ 'False => TryUnify 'False x y                        -- [3]
instance {-# INCOHERENT #-} (x ~~ y, same ~ 'True) => TryUnify same x y   -- [4]

type (~?~) :: forall a b. a -> b -> Constraint
type x ~?~ y = TryUnify (x === y) x y                                     -- [5]
```

(the snippet uses [standalone kind signatures](https://ghc-proposals.readthedocs.io/en/latest/proposals/0054-kind-signatures.html))

The idea is simple:

1. `x === y` checks whether two values are in fact the same value (it's a heterogeneous version of the [`(==)`](http://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Type-Equality.html#g:4) type-level operator from `base`, except we don't bother special-casing applications).
2. `TryUnify` then receives the result of that check as its first argument and dispatches on it.
3. If the result is `'False`, then we know that `x` and `y` are distinct and can't be unified, so we leave them as is.
4. If the result is something else, i.e. either `'True` or inconclusive, then we ask GHC to unify `x` with `y` via `x ~~ y` (where [`(~~)`](https://hackage.haskell.org/package/ghc-prim-0.6.1/docs/GHC-Types.html#t:-126--126-) is a heterogeneous version of the regular equality constraint operator `(~)`). If the result of the equality check was `'True` then we just asked GHC to unify two things that were already equal, so this is useless but harmless. Hence it's the other case that we're actually interested in: when the result of the equality check is inconclusive. Why would it be inconclusive, anyway? Well, there are options, for example `x` or `y` can reference a stuck type family, but what we're after here is handling the case where `x` or `y` reference one or more ambiguous type variables (which arise from uninstantiated polymorphism). If `x` is fully unambiguous, then force-unifying it with ambiguous `y` makes the latter also fully unambiguous (again, type families complicate the matter, but we're not interested in those details). So this is how we detect uninstatiated polymorphism and instantiate it. We'll see some examples shortly.
5. Assemble everything together by passing the result of the `x === y` check to `TryUnify`.

## Examples

Let's now look at an example. If we ask for the type of `42`, we'll get a polymorphic type:

```
>>> :t 42
42 :: Num p => p
```

Having a function that specifies its argument to be of type, say, `Int`:

```haskell
specifyAsInt :: Int -> Int
specifyAsInt = id
```

we can use it to instantiate the polymorphism:

```
>>> :t specifyAsInt 42
specifyAsInt 42 :: Int
```

Applying that function to a non-`Int` argument gives us an error, obviously:

```
>>> :t specifyAsInt True
<interactive>:1:14-17: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Bool’
    • In the first argument of ‘specifyAsInt’, namely ‘True’
      In the expression: specifyAsInt True
```

Now what the `TryUnify` thing allows us to do is to default the type of an expression to `Int` if possible and do nothing otherwise. Having

```haskell
defaultToInt :: a ~?~ Int => a -> a
defaultToInt = id
```

we can use it to instantiate polymorphism:

```
>>> :t defaultToInt 42
defaultToInt 42 :: Int
```

And it doesn't fall apart when applied to an argument of the wrong type:

```
>>> :t defaultToInt True
defaultToInt True :: Bool
```

including a polymorphic one:

```
>>> :t defaultToInt [42]
defaultToInt [42] :: Num a => [a]
```

This probably doesn't look too exciting right now, but `TryUnify` is our main building block that'll allow us to implement some quite intricate things.

As a slightly less trivial example we can implement defaulting for all ambiguous type variables in a type:

```haskell
type DefaultAllTo :: forall a b. a -> b -> Constraint
class DefaultAllTo d y                                                              -- [1]
instance (DefaultAllTo d f, DefaultAllTo d x) => DefaultAllTo d (f x)               -- [2]
instance {-# INCOHERENT #-} (DefaultAllTo d b, d ~?~ y) => DefaultAllTo d (y :: b)  -- [3]
```

Works as follows:

1. `DefaultAllTo` receives two arguments: the first one (`d`) is what to instantiate an ambiguous type variable with and the second one (`y`) is a type to instantiate all type variables in.
2. Deconstruct the type being processed into a function and its argument and proceed recursively until a leaf is reached and no deconstrucion is possible.
3. Recurse into the kind of the leaf (because it may also contain ambiguous type variables) and try to unify the default value parameter (`d`) with the leaf, thereby instantiating the leaf with `d` if the former is an ambiguous type variable.

We can check that this works nicely. Having

```haskell
defaultAllToInt :: DefaultAllTo Int a => a -> a
defaultAllToInt = id
```

it's possible to instantiate all type variables in a polymorphic type with `Int`:

```
>>> :t defaultAllToInt reverse
defaultAllToInt reverse :: [Int] -> [Int]
```

which also works when there are uninstantiable type variables (due to them having a different kind than the default value parameter), constraints etc:

```
>>> :t defaultAllToInt elem
defaultAllToInt elem :: Foldable t => Int -> t Int -> Bool
```

or dependent kinding:

```
>>> :set -fprint-explicit-foralls
>>> :t defaultAllToInt Proxy
defaultAllToInt Proxy :: forall {t :: Int}. Proxy t
```

We can instantate ambiguous type variables of arbitrary kinds. For example, having

```haskell
defaultAllToList :: DefaultAllTo [] a => a -> a
defaultAllToList = id
```

it's possible to instantiate all type variables of kind `* -> *` with `[]`:

```
>>> :t defaultAllToList fmap
defaultAllToList fmap :: (a -> b) -> [a] -> [b]
```

as well as to mix the two defaulting functions together:

```
>>> :t defaultAllToList $ defaultAllToInt fmap
defaultAllToList $ defaultAllToInt fmap
  :: (Int -> Int) -> [Int] -> [Int]
```

Finally, I keep calling the first argument of `DefaultAllTo` a "default value parameter", 'cause it doesn't have to be a type at all. It can be a, say, boolean. For example, this defaulting function:

```haskell
defaultAllToTrue :: DefaultAllTo 'True a => a -> a
defaultAllToTrue = id
```

can be used like that:

```
>>> :t defaultAllToTrue Proxy
defaultAllToTrue Proxy :: Proxy 'True
```

## How bad is this hack?

`TryUnify` uses an [`INCOHERENT`](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XOverlappingInstances) instance. How bad are they? Well, it's one of the most god-awful extensions of the language (together with `OVERLAPPING` instances). Not as terrible as `RecordWildCards` making your code write-only (sorry, I had to make this point), but still rather terrible, as incoherent instances make the order, in which constraints get solved, matter.

For one thing, `(C, D) => <...>` and `(D, C) => <...>` are no longer equivalent. For example, if instead of the current

```haskell
instance {-# INCOHERENT #-} (DefaultAllTo d b, d ~?~ y) => DefaultAllTo d (y :: b)
```

we wrote

```haskell
instance {-# INCOHERENT #-} (d ~?~ y, DefaultAllTo d b) => DefaultAllTo d (y :: b)
```


then GHC would first try to unify `d` with `y` and only then recurse into `b` (the kind of `y`). Which could actually result in a different specialization. For instance, we had this example above:

```
>>> :set -fprint-explicit-foralls
>>> :t defaultAllToInt Proxy
defaultAllToInt Proxy :: forall {t :: Int}. Proxy t
```

but if instead of recursing into the kind of a leaf first, we attempted to unify the leaf with the default value right away, we'd specialize

```
Proxy :: forall {a :: *} (x :: a). Proxy x
```

as `Proxy :: Proxy Int`, since that's what you get by unifying `x` with `Int`.

But it gets worse, for instance you can change the order of elements in a list and get a different type inferred:

```
>>> :t [defaultToInt 1, defaultToDouble 2]
[defaultToInt 1, defaultToDouble 2] :: [Int]
>>> :t [defaultToDouble 1, defaultToInt 2]
[defaultToDouble 1, defaultToInt 2] :: [Double]
```

(where `defaultToDouble` is defined analogously to `defaultToInt`).

So yes, the hack is pretty bad. It can be convenient, though. For example, `QuickCheck` considers its go-to type to be `Int` and it might make sense to preserve polymorphism in properties of polymorphic functions (for the sake of being explicit about not relying on any particular specialization) and later automatically instantiate all type variables with `Int` like that:

```haskell
prop_reverseReverse :: Eq a => [a] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

main = quickCheck $ defaultAllToInt prop_reverseReverse
```

You could even provide a `quickCheckPoly = quickCheck` function and sneak `DefaultAllTo Int` into its type.

But that's an imaginary use case. There's an actual one, which is how this posts exists: we have polymorphic built-in functions in [Plutus](https://github.com/input-output-hk/plutus) and the meaning of such a builtin is a polymorhic Haskell function and [we're able](https://github.com/input-output-hk/plutus/blob/991cce8df4a4268c883d3233b531763d39f98d3f/plutus-core/test/Evaluation/DynamicBuiltins/Definition.hs#L140-L147) to extract the Plutus type of a polymorphic built-in function only by looking at the type of the meta function by detecting polymorphism and instantiating it as appropriate. Not too important as we could make the user write the type out manually, but given that [it's laborious](https://github.com/input-output-hk/plutus/blob/c975f6d5237af707cf8cea9d289768238ee8f166/plutus-core/test/Evaluation/DynamicBuiltins/Definition.hs#L140-L151) and requires understanding of the underlying representation of type schemes, it's kinda nice to be able to infer everything automatically not only in the monomorphic case, but also in the polymorphic one.

## Conclusions

I think this all is as awful as fun. Awfun. Future posts are going to be even more awfun, this one is merely a warm-up.
