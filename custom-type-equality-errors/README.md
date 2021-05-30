# Custom type equality errors

Consider the following snippet from the readme of the [`type-level-sets`](https://hackage.haskell.org/package/type-level-sets) library:

```haskell
foo :: Map '["x" :-> Int, "z" :-> Bool, "w" :-> Int]
foo = Ext (Var :: (Var "x")) 2
    $ Ext (Var :: (Var "z")) True
    $ Ext (Var :: (Var "w")) 5     -- [1]
    $ Empty
```

If we replace `5` with, say, `False` at [1] to introduce a type error, we'll see the following:

```
error:
    • Couldn't match type ‘Bool’ with ‘Int’
      Expected type: Map '[ "x" ':-> Int, "z" ':-> Bool, "w" ':-> Int]
        Actual type: Map '[ "x" ':-> Int, "z" ':-> Bool, "w" ':-> Bool]
    • In the expression:
        Ext (Var :: (Var "x")) 2
          $ Ext (Var :: (Var "z")) True
              $ Ext (Var :: (Var "w")) False $ Empty
      <...>
```

By comparing the "Expected type" and "Actual type" lines we can figure out at which key the type mismatch has occurred, but such lines can get huge and it would be nice to be able to define custom type equality errors specifying important information directly (in our case, the key), so that the user does not need to dig it out themselves. Plus, in the above case we get to the error too late during type checking, which causes the whole expression to be printed in the error message. If the error was triggered in a more localized way, the error message would be less noisy and more to the point.

There's a way to fix both of these problems. The trick is to define a separate type class mimicking type equality constraints but having custom equality checks:

```haskell
type CheckEqualKV :: forall k v. k -> k -> v -> v -> Constraint
type family CheckEqualKV k1 k2 v1 v2 where
    CheckEqualKV k1 k1 v1 v1 = ()
    CheckEqualKV k1 k2 v1 v2 =
        TypeError
            ( 'ShowType (k1 ':-> v1) ':<>:
              'Text " is not equal to " ':<>:
              'ShowType (k2 ':-> v2)
            )

type EqualKV :: forall k v. k -> k -> v -> v -> Constraint
class (k1 ~ k2, v1 ~ v2) => EqualKV k1 k2 v1 v2
instance (CheckEqualKV k1 k2 v1 v2, k1 ~ k2, v1 ~ v2) => EqualKV k1 k2 v1 v2
```

Here `EqualKV k1 k2 v1 v2` is almost the same thing as its superclass constraint `(k1 ~ k2, v1 ~ v2)`, except the former in addition to trying to unify `k1` with `k2` and `v1` with `v2` also invokes `CheckEqualKV k1 k2 v1 v2` at use site which triggers a custom type error whenever `k1` is not equal to `k2` or `v1` is not equal to `v2`.

We only need to define an `EqualKV`-powered version of `Ext`:

```haskell
-- cf.: Ext :: Var k -> v -> Map m -> Map ((k :-> v) ': m)
ext :: EqualKV k1 k2 v1 v2 => Var k1 -> v1 -> Map m -> Map ((k2 :-> v2) ': m)
ext = Ext
```

and we can check that using it in the definition of `foo`

```haskell
foo :: Map '["x" :-> Int, "z" :-> Bool, "w" :-> Int]
foo = ext (Var :: (Var "x")) 2
    $ ext (Var :: (Var "z")) True
    $ ext (Var :: (Var "w")) False
    $ Empty
```

indeed gives us a more comprehensible error message:

```
error:
    • "w" ':-> Bool is not equal to "w" ':-> Int
    • In the expression: ext (Var :: (Var "w")) False
      <...>
```

Note how the offending subexpression is well pinpointed.

And we can extend `CheckEqualKV` with an additional clause when the two keys match and their values don't:

```haskell
type family CheckEqualKV k1 k2 v1 v2 where
    CheckEqualKV k1 k1 v1 v1 = ()
    CheckEqualKV k1 k1 v1 v2 =
        TypeError
            ( 'ShowType v1 ':<>:
              'Text " is not equal to " ':<>:
              'ShowType v2 ':<>:
              'Text " at " ':<>:
              'ShowType k1
            )
    <the_old_clause>
```

to get an even more comprehensible error message:

```
error:
    • Bool is not equal to Int at "w"
    • In the expression: ext (Var :: (Var "w")) False
	<...>
```

Note that `EqualKV k1 k2 v1 v2` has `(k1 ~ k2, v1 ~ v2)` as its superclass constraint, which is no worse in terms of inference than substituting `k1` for `k2` and `v1` for `v2` directly in a type signature and so that `EqualKV` trick does not break inference. We can check that: the type of the following expression is still perfectly inferred:

```haskell
-- >>> :t bar
-- bar :: Map '[ "x" ':-> Int, "z" ':-> Bool, "w" ':-> Int]
bar = ext (Var :: (Var "x")) (2 :: Int)
    $ ext (Var :: (Var "z")) True
    $ ext (Var :: (Var "w")) (5 :: Int)
    $ Empty
```

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
