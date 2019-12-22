# `HasLens` done right

**EDIT**: as [it turned out](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-568217485) the solution presented here has the same type inference capabilities as the functional dependencies one. This was pointed out in this [response](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-568206301).

However the fundeps solution, as the type families one, can directly handle [neither phantoms types, nor type families](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-568218989) unlike the `SameModulo` solution. But it can handle them indirectly.

I'll clean the post up once I'm less tired (code already updated), but long story short: the novel solution presented here is kind of more expressive than existing solutions, but that additional expressiveness can be covered by introducing some indirections.

## Preface

The title is a bit clickbaity, I do not really know whether the solution presented in this post is "done right" or not. But so far it does seem to perform better than widely known approaches. Jump straight to [Conclusions](https://github.com/effectfully/sketches/tree/master/has-lens-done-right#conclusions) if you're only interested in what makes the new approach better. See also [this response](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-568206301) for how they can achieve all the same benefits that the new machinery provides by implementing special rules regarding the `HasLens` class in the compiler. They've also got better type inference with one of the old approaches, but I currently do not undestand how they've managed to do that and whether their solution is sufficient.

For general context, read the [`overloaded-record-fields`](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst) and (especially) [`record-set-field`](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) proposals.

This post first describes known approaches to constructing a `HasLens` class that allows to retrieve a `Lens` into a type (most commonly the lens is focused on a field of the type and is retrieved by the name of the field). Then I outline a new possible solution and show how examples that are troubling for other solutions can be handled with the new one.

This post builds on the ideas from one of my previous posts: [`poly-traversable`](https://github.com/effectfully/sketches/tree/master/poly-traversable) where I suggested that the technique developed there can be used for solving the records problem. Reading that post is not a prerequisite though as this writing is self-contained and I improved the technique after Adam Gundry [pointed out](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-542589304) that the previous machinery didn't allow for poly-kinded update.

## Known approaches to the `HasLens` problem

Monomorphic lenses:

- [an ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/sorf#record-updates)
- [`Control.Lens.TH.makeFields`](http://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-TH.html#v:makeFields)
- [`data-has`](http://hackage.haskell.org/package/data-has-0.3.0.0/docs/Data-Has.html#t:Has)
- [`can-i-haz`](http://hackage.haskell.org/package/can-i-haz-0.3.1.0/docs/Control-Monad-Reader-Has.html#t:Has)
- [`has`](https://github.com/nonowarn/has/blob/225931d880efadd433bb18d3c6163f2ff01ba120/src/Data/Has.hs#L79)
- [`Data.Generics.Product.Fields.HasField'`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField-39-)

Polymorphic lenses + a class with functional dependencies:

- [an ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/magic-classes#design)
- [`Control.Lens.Tuple`](https://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Tuple.html)
- [`Optics.Label`](https://hackage.haskell.org/package/optics-core-0.2/docs/Optics-Label.html#t:LabelOptic)
- [`Record`](https://github.com/nikita-volkov/record/blob/e534886eaed0e1179eb6fe6d73ec08e2dd26f521/library/Record.hs#L32)
- [a comment in the RST thread](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449590983)
- [a response to ORF](https://raw.githubusercontent.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)
- [an SO answer](https://stackoverflow.com/a/34974164/3237465)

Polymorphic lenses + a class with functional dependencies + cleverness to get decent type inference:

- [`Data.Generics.Product.Fields.HasField`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField)

Incoherent mix between the last two:

- [`Data.Generics.Labels.Field`](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Labels.html#t:Fields)

Polymorphic lenses + type families:

- [another ORF page](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#record-field-constraints)
- [a response to ORF](https://raw.githubusercontent.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)
- [`records-prototype`](https://github.com/adamgundry/records-prototype/blob/master/RecordsPrototype.hs)

The lists of links above do not meant to be exhaustive, they're just to give you an idea of how common each approach is and reference a few existing implementations.

## A bit of history

_It seems_ that [originally](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design) the plan was to implement in GHC the type families approach. Then they [moved](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/magic-classes) to the functional dependencies approach, but somehow [ended up](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) accepting the no polymorphism approach.

So let's discuss all of these.

## No polymorphism

One possible approach is to simply forbid polymorphism. That might be an acceptable thing for a library, but wiring such a machinery into the compiler is a non-solution. Anything that doesn't have a story for polymorphism, doesn't really make us any closer to solving the records problem, because polymorphism is a must as it's ubiquitous in Haskell code and the hard part of the records problem is how to handle polymorphism -- not how to define a bunch of trivial monomorphic type classes and wire them into the compiler, just because some people said they've been using this machinery in production (the public part of which is [pretty much irrelevant and the entire use case is not representative](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-542767516)).

Hence the hard problem of handling polymorphism should drive the research, not [arguments](https://github.com/ghc-proposals/ghc-proposals/pull/158) like

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

The reason why we're using monomorphic data is that all the approaches seem to work equally well for polymorphic data. So even though the functional dependencies approach has its problems, the [`Control.Lens.Tuple`](https://hackage.haskell.org/package/lens-4.18.1/docs/Control-Lens-Tuple.html) use case seems to be a good fit for handling it with functional dependencies.

Lens types (like `Lens`, `Lens'`, etc) and operators (like `(%~)`, `(.~)`, etc) are taken from the `microlens` package, i.e. they are fully compatible and interchangeable with the ones from `lens`.

## Functional dependencies ([full code](src/FunDep.hs))

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

But the major problem of this representation is that it breaks type inference (but see the **EDIT** in the beginning of the file). Consider this example:

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

Note that [Data.Generics.Product.Fields](https://hackage.haskell.org/package/generic-lens-1.2.0.1/docs/Data-Generics-Product-Fields.html#t:HasField) does something different: it provides a single instance (modulo an additional instance that is irrelevant for this discussion) that looks like this:

```haskell
instance  -- see Note [Changing type parameters]
  ( HasTotalFieldP field (Rep s) ~~ 'Just a
  , HasTotalFieldP field (Rep t) ~~ 'Just b
  , HasTotalFieldP field (Rep (Indexed s)) ~~ 'Just a'
  , HasTotalFieldP field (Rep (Indexed t)) ~~ 'Just b'
  , t ~~ Infer s a' b
  , s ~~ Infer t b' a
  , HasField0 field s t a b
  ) => HasField field s t a b where
  field f s = field0 @field f s
```

where

```haskell
class HasField (field :: Symbol) s t a b | s field -> a, t field -> b, s field b -> t, t field a -> s where
  field :: VL.Lens s t a b

class HasField0 (field :: Symbol) s t a b where
  field0 :: VL.Lens s t a b
```

The note that the instance refers to is [this one](https://github.com/kcsongor/generic-lens/blob/17e35a237bde0f9599578e62936426936c4dbfc5/src/Data/Generics/Internal/Families/Changing.hs#L25).

The machinery looks clever, but I don't know how good it's in terms of type inference and whether it has any edge cases or clutters type signatures or has any other disadvantages.

## Type families ([full code](src/TF.hs))

The type families approach ([code taken](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#limited-type-changing-update) directly from GHC wiki) looks like this:

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

Here "the type at `n` in `r` is `a`" is implemented as the `FldTy` type family.

`UpdTy r n a` reads as "replace the type at `n` in `r` with `a`".

This type signature:

```haskell
    setField :: Proxy# n -> r -> t -> UpdTy r n t
```

reads as "replace the value at `n` in an `r` with a `t`", which of course implies that the type at `n` in `r` has to be replaced with `t`, because it might be a different type than the one that `r` holds at `n`. And if it's not a different type, then we have the `r ~ UpdTy r n (FldTy r n)` constraint, which automatically turns the type of `setField` into

```haskell
    setField :: Proxy# n -> r -> t -> r
```

in this case. Which is just a regular monomorphic setter.

The `User` example looks like this:

```haskell
type instance FldTy User "name" = String
type instance UpdTy User "name" String = User

instance t ~ String => Has User "name" t where
    getField _ (User _ name) = name

instance t ~ String => Upd User "name" t where
    setField _ (User email _) name = User email name
```

and if we check how types get inferred:

```haskell
test0 :: _
test0 f = User "john@gmail.com" "John" & lens @"name" %~ f
```

we'll see

```
    • Found type wildcard ‘_’ standing for ‘([Char] -> [Char]) -> User’
```

I.e. everything got inferred correctly.

Unfortunately, types only get inferred in a bottom-up fashion. I.e. if the type of the record being updated is known, then the compiler will infer the type of the function used for updating the record. But if the type of the result is known as well as the type of the updating function, then the type of the record being updated won't be inferred. I.e.

```haskell
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"
```

results in

```
    • Found type wildcard ‘_’ standing for ‘s0’
      Where: ‘s0’ is an ambiguous type variable
```

We could probably make type inference top-down rather than bottom-up (i.e. make the former case break and the latter work), which I believe is a better practice, but in any case type inference is unidirectional with the type families approach, which is a limitation.

Note also that if we wanted to encode the fancy apotheosis and user-killing examples from the previous section, we could also do that, e.g.

```haskell
type instance FldTy User "name" = String
type instance UpdTy User "name" String = User
type instance UpdTy User "name" () = NamelessGod

instance t ~ String => Has User "name" t where
    getField _ (User _ name) = name

instance Upd User "name" String where
    setField _ (User email _) name = User email name

instance Upd User "name" () where
    setField _ (User email _) () = NamelessGod email
```

See more examples in [`records-prototype`](https://github.com/adamgundry/records-prototype/blob/master/RecordsPrototype.hs).

The main problems of this approach are:

- unidirectional type inference
- [phantom types do not work](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-phantom-arguments)
- [type families in certain cases do not work](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-type-families)

## [The multiple type-changing updates problem](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-multiple-fields)

This limitation applies to all the approaches mentioned in this documented (including the `SameModulo` one). Possible ways to handle multiple type-changing updates are

- [explicitly](https://github.com/ghc-proposals/ghc-proposals/pull/6#discussion_r78147352)
- [via tuples](http://r6.ca/blog/20120623T104901Z.html)
- [via linearization](https://raw.githubusercontent.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)

But then there is another question, in `a { x = x', y = y' }` do we want to call `setField` twice instead of taking the product of corresponding setters? It would be nice if we could generically update the entire record at once regardless of whether some updates are type-changing or not. And if we could do that, then this would also solve the multiple type-changing updates problem.

Anyway, the first two solutions from the above list look fine.

## [Fields with rank-n types](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#higher-rank-fields)

Fields with rank-n types are generally troubling for any of the approaches (note that type-changing update is completely orthogonal).

## The `SameModulo` approach ([full code](src/Main.hs))

Here is the core of the novel `SameModulo` approach:

```haskell
type family Get (x :: k) s

class SameModulo x t s => SameModulo (x :: k) s t where
    lensAt :: (a ~ Get x s, b ~ Get x t) => Proxy# x -> Lens s t a b
```

We have the `Get` type family that allows to get the type of the `x` field in `s` just like in the type families approach and we have the `SameModulo` class that allows to retrieve a lens focused on the `x` field of `s`, whose type is naturally `Get x s`, which we abbreviate as `a`.

Whenever `SameModulo x s t` holds, `SameModulo x t s` must also hold as the `SameModulo x t s => ...` constraint indicates. I.e. `SameModulo x` is a symmetric relation by definition.

`SameModulo` is an internal type class and we need some convenient user-facing API. There are choices, but here is the simplest one:

```haskell
class (SameModulo x s t, a ~ Get x s, b ~ Get x t) => HasLens x s t a b
instance (SameModulo x s t, a ~ Get x s, b ~ Get x t) => HasLens x s t a b

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#
```

`HasLens x s t a b` is pretty much a class alias for `SameModulo x s t`, except the `a` and `b` variables are explicit in the former. This makes type signatures nicer and I personally ([not only](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449422693)) find [error messages](https://github.com/ghc-proposals/ghc-proposals/pull/158#issuecomment-449419429) more to the point.

### The `User` example

Providing an instance for `User` is trivial:

```haskell
type instance Get "name" User = String
instance t ~ User => SameModulo "name" User t where
    lensAt _ f (User email name) = User email <$> f name
```

Unlike with the other approaches, type inference is bidirectional:

```haskell
-- Found type wildcard ‘_’ standing for ‘([Char] -> String) -> User’
test0 :: _
test0 f = User "email" "name" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘User’
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"
```

In both the cases types are inferred correctly.

### Polymorphism

Providing an instance for tuples is also trivial (the `_1` function comes from the `microlens` package)

```haskell
type instance Get "_1" (a, b) = a
instance t ~ (a', b) => SameModulo "_1" (a, b) t where
    lensAt _ = _1
```

Type changing update works:

```haskell
-- Found type wildcard ‘_’ standing for ‘((Int, Bool), Char)’
polyTupleTest :: _
polyTupleTest = (("abc", True), 'd') & lens @"_1" . lens @"_1" %~ length
```

Type signatures for general combinators look nicely (when written by hand. Inference for polymorphic things does not work and even if it worked, it probably would infer something not nice at all):

```haskell
poly
    :: (HasLens "_1" s t sa tb, HasLens "_1" sa tb a b)
    => Lens s t a b
poly = lens @"_1" . lens @"_1"
```

### [The phantom arguments problem](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-phantom-arguments) is solved:

```haskell
data Ph (a :: k) (bs :: [Bool]) = Ph { foo :: Int }

type instance Get "foo" (Ph a b) = Int
instance t ~ Ph (a' :: k') bs' => SameModulo "foo" (Ph a b) t where
    lensAt _ f (Ph i) = Ph <$> f i

ph :: Lens (Ph (a :: k) bs) (Ph (a' :: k') bs') Int Int
ph = lens @"foo"
```

Note that we have poly-kinded update (`a :: k` to `a' :: k'`).

### [The type families problem](https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-type-families) is solved:

```haskell
type family Goo (a :: k)
data Tf (a :: k) = Tf { bar :: Goo a }

type instance Get "bar" (Tf a) = Goo a
instance t ~ Tf (a' :: k') => SameModulo "bar" (Tf (a :: k)) t where
    lensAt _ f (Tf x) = Tf <$> f x

tf :: Lens (Tf (a :: k)) (Tf (a' :: k')) (Goo a) (Goo a')
tf = lens @"bar"
```

Note that we have poly-kinded update under a type family (`a :: k` to `a' :: k'`).

## Conclusions

So the `SameModulo` approach

- compared to the monomorphic version: does the job
- compared to the version with functional dependencies: more clutter and unsafety, doesn't fall apart on phantom types and type families.
- compared to the version with type families: type inference is not half-broken, less noise, doesn't fall apart on phantom types and type families.
