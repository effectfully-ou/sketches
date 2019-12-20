# `HasLens` done right


monorphic:

- [an ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/sorf#record-updates)
- [`Control.Lens.TH.makeFields`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-TH.html#v:makeFields)
- [`data-has`](http://hackage.haskell.org/package/data-has-0.3.0.0/docs/Data-Has.html#t:Has)
- [`can-i-haz`](http://hackage.haskell.org/package/can-i-haz-0.3.1.0/docs/Control-Monad-Reader-Has.html#t:Has
- [`has`](https://github.com/nonowarn/has/blob/225931d880efadd433bb18d3c6163f2ff01ba120/src/Data/Has.hs#L79)
- [`Data.Generics.Product.Fields.HasField'`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField-39-))

functional dependencies:

- [an ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/magic-classes#design)
- [`Control.Lens.Tuple`](https://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Tuple.html)
- [a comment in the RST thread](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449590983)
- [a response to ORF](https://raw.githubusercontent.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)
- [an SO answer](https://stackoverflow.com/a/34974164/3237465)

functional dependencies + cleverness:

- [`Data.Generics.Product.Fields.HasField`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField)

incoherent mix:

- [`Data.Generics.Labels.Field`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Labels.html#t:Fields)

type families:

- [another ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#record-field-constraints)
- [a response to ORF](https://raw.githubusercontent.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)

## No polymorphism

So one possible approach is to simply forbid polymorphism. That might be an acceptable thing for a library, but wiring such a machinery into the compiler is a non-solution. Anything that doesn't have a story for polymorphism, doesn't really make us any closer to solving the records problem, because polymorphism is a must as it's ubiquitous in Haskell code and the hard part of the records problem is how to handle polymorphism -- not how to define a bunch of trivial monomorphic type classes and wire them into the compiler, just because some people said they've been using this machinery in production (the public part of which is [pretty much irrelevant and the entire use case is not representative](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-542767516)) and refused to give examples upon a request ([because closed source](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-542822673)) (not to say that what they've been using in production is quite different to what was in the original proposal and even would break that production). And what kind of argument is that? "We've been using this in production". People use Go in production and many are quite happy with it, even though from a PLT perspective that language is a joke.

So the hard problem of handling polymorphism should drive the research, not [arguments](https://github.com/ghc-proposals/ghc-proposals/pull/158) like

> Q: Should we allow type changing lenses? No - this results in additional implementation complexity. Let's aim to get something through, rather than nothing, and a future dedicated soul can extend it.

that pretend there is a clear way to extend the monomorphic approach, which is not the case. "Extending" the monomorphic approach would very likely change the whole thing entirely, so I don't see any reason to commit to that approach in the first place and I'm very disappointed by the fact that the committee approved the monomorphic solution, even though not only does it forbid polymorphism, but also [doesn't support a bunch of nice use cases](https://github.com/ghc-proposals/ghc-proposals/pull/158) and [falls apart on a single `Maybe`](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-567766069).

One additional argument that I want to address here, because it keeps popping up despite being completely invalid is [this one](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-541447553):

> A number of people have bemoaned the loss of type-changing record update. I, too, am saddened by this potential loss. But: do you have an example of real-world code that uses this feature? Note that my question is not: could you come up with such an example, but do you already have an example of it? I wouldn't want a almost-never-used feature such as type-changing update to torpedo this proposal which is otherwise very useful.

I responded with

> 1. This is not a valid question to ask as the current record update syntax is nearly useless and is not used as often as it could be. This proposal aims at improving the situation and so it can't refer back to the nearly useless solution taking a particular instance of that uselessness as evidence of a certain feature not being commonly used
> 2. Having said that, I'll answer your question: I use the record update syntax only for defining lenses manually (which admittedly doesn't happen too often, but it's a valid use case) and that requires type-changing update for polymorphic records

So let me repeat: anything that doesn't have a story for polymorphism, doesn't really make us any closer to solving the records problem.

## Testing example

We will be using the following two monomorphic data types for testing polymorphic approaches:

```haskell
data User = User
    { userEmail :: String
    , userName  :: String
    }

data NamelessGod = NamelessGod
    { namelessGodEmail :: String
    }
```

The reason why we're using monomorphic data is that the functional dependencies and the `SameModulo` approaches seem to work equally well for polymorphic data. I.e. even though the functional dependencies approach has its problems, the [`Control.Lens.Tuple`](https://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Tuple.html) use case seems to be a good fit for handling it with functional dependencies.

I believe, the type families approach has troubles with polymorphic data w.r.t. type inference, but it's really such a bad approach that it has enough problems even without considering this particular edge case.

Lens types (like `Lens`, `Lens'`, etc) and operators (like `(%~)`, `(.~)`, etc) are taken from the `microlens` package, i.e. they are fully compatible and interchangeable with the ones from `lens`.

## Functional dependencies

The functional dependencies approach looks like this:

```haskell
class HasLens (x :: k) s t a b
            | x s -> a    -- the type at `x` in `s` is `a`
            , x t -> b    -- the type at `x` in `t` is `b`
            , x s b -> t  -- if you replace the type at `x` in `s` with `b`, you'll get `t`
            , x t a -> s  -- if you replace the type at `x` in `t` with `a`, you'll get `s`
            where
    lensAt :: Proxy# x -> Lens s t a b

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#
```

Short and sweet. Making instances is trivial:

```haskell
instance HasLens "name" User User String String where
    lensAt _ f (User email name) = User email <$> f name
```

But the major problem of this representation is that it breaks type inference. Consider this example:

```haskell
test = User "john@gmail.com" "John" & lens @"name" %~ _
```

GHC reports

```
    • Found hole: _ :: String -> b1
      Where: ‘b1’ is an ambiguous type variable
```

while what we'd like to see is

```
    • Found hole: _ :: String -> String
```

So GHC can't resolve `b1` as `String`. Why is that? Because it doesn't have to be `String`! The comments in

```
            , x s b -> t  -- if you replace the type at `x` in `s` with `b`, you'll get `t`
            , x t a -> s  -- if you replace the type at `x` in `t` with `a`, you'll get `s`
```

are not accurate and describe what we'd like to get, but what we actually have is merely that `x`, `s` and `b` together determine `t`, which means that `s` and `t` do not have to be the same type modulo `x`, they can be completely different types and the semantics of a `HasLens` instance can be anything in this case.

For example we can define this instance:

```haskell
instance HasLens "name" User NamelessGod String () where
    lensAt _ f (User email name) = NamelessGod email <$ f name
```

and turn a user being into a nameless god using the `(.~)` operator:

```haskell
apotheosis :: NamelessGod
apotheosis = User "john@gmail.com" "John" & lens @"name" .~ ()
```

Or we can kill a user by writing their name in a Death Note:

```haskell
instance HasLens "name" User Void String Void where
    lensAt _ f (User _ name) = f name

type DeathNote = String -> Void

writeIn :: DeathNote -> User -> Void
writeIn kill user = user & lens @"name" %~ kill
```

This use cases are rather weird and we have to pay by having broken type inference in order to support them. The bad thing here is that with the functional dependencies approach there is no way to define an instance without supporting this use case, i.e. type inference is broken generally for all monomorphic data types. This is a huge price to pay: broken type inference doesn't mean that you won't be able to leave top-level definitions without type signatures as you shouldn't do that anyway -- the problem is that you'll occasionally get weird errors about ambiguous types at the call site. Finding what causes such errors is rather annoying, especially when you wrote something that does make perfect sense and you do not think of your code as being type ambiguous.

Additionally, two machineries with weak type inference won't compose without explicit types sprinkled over the code. Anything that goes into the compiler had better be as inference-friendly as possible as that allows libraries to cut some corners when they need that and make not very inference-friendly APIs.

See the [full code](src/FunDep).

[Data.Generics.Product.Fields](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField)

```haskell
(HasTotalFieldP field (Rep s) ~~ Just a, HasTotalFieldP field (Rep t) ~~ Just b, HasTotalFieldP field (Rep (Indexed s)) ~~ Just a', HasTotalFieldP field (Rep (Indexed t)) ~~ Just b', t ~~ Infer s a' b, s ~~ Infer t b' a, HasField0 field s t a b) => HasField field s t a b
```

## Type families

```haskell
type family FldTy (r :: *) (n :: Symbol) :: *

class t ~ FldTy r n => Has r (n :: Symbol) t where
    getField :: Proxy# n -> r -> t

type family UpdTy (r :: *) (n :: Symbol) (a :: *) :: *

class (Has r n (FldTy r n), r ~ UpdTy r n (FldTy r n)) =>
            Upd (r :: *) (n :: Symbol) (t :: *) where
    setField :: Proxy# n -> r -> t -> UpdTy r n t

lens
    :: forall n s t a b. (Upd s n b, t ~ UpdTy s n b, a ~ FldTy s n)
    => Lens s t a b
lens = Lens.lens (getField pn) (setField pn) where
    pn :: Proxy# n
    pn = proxy#
```

```haskell
type instance FldTy User "name" = String
type instance UpdTy User "name" String = User

instance t ~ String => Has User "name" t where
    getField _ (User _ name) = name

instance t ~ String => Upd User "name" t where
    setField _ (User email _) name = User email name
```

```haskell
-- Found type wildcard ‘_’ standing for ‘([Char] -> [Char]) -> User’
test0 :: _
test0 f = User "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘s0’
-- Where: ‘s0’ is an ambiguous type variable
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"
```



```haskell
type instance FldTy User' "name" = String
type instance UpdTy User' "name" String = User'
type instance UpdTy User' "name" () = NamelessGod

instance t ~ String => Has User' "name" t where
    getField _ (User _ name) = name

instance Upd User "name" String where
    setField _ (User email _) name = User email name

instance Upd User "name" () where
    setField _ (User email _) () = NamelessGod email
```

```haskell
-- Found type wildcard ‘_’
--   standing for ‘([Char] -> b0) -> UpdTy User' "name" b0’
-- Where: ‘b0’ is an ambiguous type variable
test0' :: _
test0' f = User' "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘NamelessGod’
apotheosis :: _
apotheosis = User' "john@gmail.com" "John" & lens @"name" .~ ()
```

##

possible solution to the multiple type-changing updates problem:

- [handle them explicitly](https://github.com/ghc-proposals/ghc-proposals/pull/6#discussion_r78147352)
- [handle them via tuples](http://r6.ca/blog/20120623T104901Z.html)

## The `SameModulo` approach

```haskell
type family Get (x :: k) s

class SameModulo x t s => SameModulo (x :: k) s t where
    mkLensAt :: (a ~ Get x s, b ~ Get x t) => Proxy# x -> Lens s t a b
```


```haskell
class (SameModulo x s t, a ~ Get x s, b ~ Get x t) => HasLens x s t a b where
    lensAt :: Proxy# x -> Lens s t a b

instance (SameModulo x s t, a ~ Get x s, b ~ Get x t) => HasLens x s t a b where
    lensAt = mkLensAt

type HasLens' x s a = HasLens x s s a a

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#
```


```haskell
data User = User
    { userEmail :: String
    , userName  :: String
    }

type instance Get "name" User = String
instance t ~ User => SameModulo "name" User t where
    mkLensAt _ f (User email name) = User email <$> f name
```


```haskell

type instance Get "_1" (a, b) = a
instance t ~ (a', b) => SameModulo "_1" (a, b) t where
    mkLensAt _ f (x, y) = (, y) <$> f x

type instance Get "_1" (a, b, c) = a
instance t ~ (a', b, c) => SameModulo "_1" (a, b, c) t where
    mkLensAt _ f (x, y, z) = (, y, z) <$> f x
```

```haskell
-- Found type wildcard ‘_’ standing for ‘((Int, Bool), Char)’
polyTupleTest :: _
polyTupleTest = (("abc", True), 'd') & lens @"_1" . lens @"_1" %~ length
```

```haskell
poly
    :: (HasLens "_1" s t sa tb, HasLens "_1" sa tb a b)
    => Lens s t a b
poly = lens @"_1" . lens @"_1"
```


```haskell
-- Found type wildcard ‘_’ standing for ‘([Char] -> String) -> User’
test0 :: _
test0 f = User "email" "name" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘User’
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"
```


### [The phantom arguments problem](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-phantom-arguments) is solved :

```haskell
data Ph (a :: k) (bs :: [Bool]) = Ph { foo :: Int }

type instance Get "foo" (Ph a b) = Int
instance t ~ Ph (a' :: k') bs' => SameModulo "foo" (Ph a b) t where
    mkLensAt _ f (Ph i) = Ph <$> f i

ph :: Lens (Ph (a :: k) b) (Ph (a' :: k') d) Int Int
ph = lens @"foo"
```

### [The type families problem](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-type-families) is solved:

```haskell
type family Goo (a :: k)
data Tf (a :: k) = Tf { bar :: Goo a }

type instance Get "bar" (Tf a) = Goo a
instance t ~ Tf (a' :: k') => SameModulo "bar" (Tf (a :: k)) t where
    mkLensAt _ f (Tf x) = Tf <$> f x

tf :: Lens (Tf (a :: k)) (Tf (a' :: k')) (Goo a) (Goo a')
tf = lens @"bar"
```
