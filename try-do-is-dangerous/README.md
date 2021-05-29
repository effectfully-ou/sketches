# `Try.do` is dangerous

This post is a response to [`Try.do` for recoverable errors in Haskell](https://chrisdone.com/posts/try-do). I'll describe why I believe it's a bad idea to do what the post suggests.

## Preface

The purpose on an effect system is to allow the programmer to track effects in their code and mash them together. There are lots of axes in the design space of effect systems:

- Performance: `mtl` is pretty fast (but [not nearly as fast as we'd like it to be](https://www.youtube.com/watch?v=0jI-AlWEwYI)), extensible effect systems are usually slower, but some (e.g. [`fused-effects`](https://hackage.haskell.org/package/fused-effects-0.1.1.0)) come very close and we have hopes (see [`eff`](https://github.com/hasura/eff)) for having an effect system that is faster than `mtl`. Besides, compilation speed is an important factor as well.
- Definition-site boilerplate: how much code does the programmer need to write in order to introduce a new effect? For `mtl` -- plenty, due to the O(n*m) number of instances problem, with extensible effect systems -- less, but how much less depends on the expressiveness of the system and whether it's designed to be a low-boilerplate one (like [`polysemy`](https://reasonablypolymorphic.com/blog/freer-higher-order-effects/)) in the first place.
- Use-site boilerplate: how much support code does the programmer need to write for an effect system to function properly? Coding with `transformers` with no `mtl` on top requires lots of `lift`ing. Another example is extensible effect systems often having bad type inference and thus requiring explicit non-top-level type annotations.
- Expressiveness: [`freer-simple`](https://hackage.haskell.org/package/freer-simple) allows only for first-order effects, while, say, `mtl` is perfectly comfortable with continuations and whatnot. And [`unliftio`](https://hackage.haskell.org/package/unliftio) supports only a very few effects.
- Correct-by-construction compositionality: it's easy to shoot yourself in the foot with [`monad-control`](http://blog.ezyang.com/2012/01/monadbasecontrol-is-unsound) or [`polysemy`](https://old.reddit.com/r/haskell/comments/dm3wyd/wip_eff_screaming_fast_effects_for_less_the/f4y4gjb/), while the aforemetioned `unliftio` completely frees the programmer from the burden of pondering cross-interactions of different effects.

So there are trade-offs involved and different contenders were designed with different priorities in mind. `monad-control` is powerful but dangerous, `unliftio` is restrictive but safe. I'm going to argue that `unliftio` + `Try.do` is both restrictive and dangerous, which does not make it an appealing way of managing effects.

## The actual response

When designing a library that provides functions running in `IO` there's always a choice of how the types should look like. Should functions return `ExceptT Error IO Result`? Or `IO (Either Error Result)`? Or just `IO` with an `Error` being thrown exceptionally? This is a highly debatable topic (for example, some people have [strong opinions](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell) regarding mixing `IO` with `ExceptT` or `Either`) and we're not going down this rabbit hole, but I just want to mention that

1. "I hate exceptions" is not a good argument for providing an API where functions catch all possible exceptions. In the presence of asynchronous exceptions this imposes the burden of rethrowing the asynchronous exception on the user, which is impossible, given that any exception can be thrown asynchronously and so you just can't tell how an exception was thrown. See [Asynchronous exception handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/) for details.
2. There does exist at least one argument in favor of mixing `IO` with `ExceptT` or `Either`: it might make sense to report errors specific to the domain of the library explicitly to ensure the programmer handles them in a specific way, while letting general `IOException`s bubble up and be handled generally (possibly by letting them kill the application/thread). There does exist a qualitative difference between domain-specific errors and general ones like "can't access network", so differentiating between these two kinds of errors may well make sense.

So I'm not hating on the `IO (Either Error Result)` pattern. This post is meant to criticize only turning this pattern into a framework.

Okay, enough chit-chat, here's an excerpt form the original post just to save you a click:

> Enter [QualifiedDo](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html), which will be available on the 9.0.1 version of GHC. What this would allow us to do is rebind `(>>=)` to mean what we’d like:
>
> ```haskell
> module Try ((Try.>>=)) where
> (>>=) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
> (>>=) m f = do
>   result <- m
>   case result of
>     Left e -> pure (Left e)
>     Right a -> f a
> ```
>
> We put this in a module called `Try` and import it with `QualifiedDo` enabled.
>
> Now our code becomes:
>
> ```haskell
> Try.do constraints <- constrainRenamed renamed
>        solved <- solveConstraints constraints
>        generalised <- generaliseSolved solved
>        resolveGeneralised generalised
> ```
>
> where each action’s type is `SomeThing -> IO (Either ErrorType OtherThing)`.
>
> <...>
>
> What did we gain? We can have our cake and eat it too. We get a trivial, syntactically-lightweight, way to string possibly-failing actions together, while retaining all the benefits of being an unliftio-able monad.
>
> Verdict: Best of all worlds.

It's true that `Try.do` is convenient for chaining functions of type `SomeThing -> IO (Either ErrorType OtherThing)`, but so is using `ExceptT`. Especially given that all the functions used are controlled by the same programmer, so they could've been defined in `ExceptT` in the first place. "But that is not `unliftio`-able" -- right, and it's a good thing, we'll come to this later.

`Try.do` is syntax sugar, not an abstraction, and as such it's a bad building block and you don't get any interop with the rest of Hackage. For example, you can't `traverse` a list with a `A -> IO (Either E B)` function and get short-circuiting. This is not only a performance concern, later actions may rely on previous ones running successfully and misbehave otherwise. Nor does `forever`, `replicateM` or anything else work correctly with `IO (Either E A)` when that `Either` is meant to carry error-handling semantics (as opposed to being merely a value).

`Try.do` is only convenient when all functions return `Either` within `IO`. If one of them returns an `IO A`, you have to explicitly lift `A` into `Either E A` It may seem like a slight inconvenience, but here's how it can be dangerous: you get used to converting `A` into `Either E A` whenever you're in `Try.do` and something does not type check, then you run

```
timeout (someAction :: IO (Either E A))
```

within `Try.do`, which does not type check, because `timeout` wraps the resulting value in `Maybe`:

```haskell
timeout :: Int -> IO a -> IO (Maybe a)
```

so you get `IO (Maybe (Either E A))` and use your reflexes to lift that into `IO (Either E (Maybe (Either E A))` instead of using `sequence` over the resulting value.

Too contrived of an example? Here's another one:

```haskell
try :: Exception e => IO a -> IO (Either e a)
```

Can you imagine running

```
try (someAction :: IO (Either E A))
```

within `Try.do` and not realizing that this makes the wrong `Either` come first? You won't even get a type error (provided `E` implements `Exception`) if you do something with the result. The whole thing will just misbehave in runtime.

What's worse in the original post is that inherently unsafe (compositionality-wise) `Try.do` gets mixed with `unliftio` designed specifically to be safe. You use this pattern of structuring everything as `MonadUnliftIO m => m (Either E A)` where `Either` carries the error-handling semantics and then you want to use it as if it was a normal `IO` action returning some result. So you do

```
someAction1 `finally` someAction2
```

`someAction2` returns an error and the error gets ignored as this is what the type signature of `finally` says:

```haskell
finally :: MonadUnliftIO m => m a -> m b -> m a
```

And in case you think it's too stupid of a mistake to make, can you immedatiely see how such error handling works in

```haskell
bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
```

? It's way too easy to reach for `bracket` without thinking too much on what happens with those hidden errors.

`unliftio` saves you from any of this by prohibiting any [sane](https://github.com/fpco/unliftio/issues/68#issuecomment-727255763) `MonadUnliftIO` instance of `ExceptT`. And there's no point in mixing a safe but restrictive library with a shallow (as in "shallow" vs "deep" embedding) unsafe trick into a single weak (as in "not expressive") spoiled framework: either stick to safety or sacrifice it for expressiveness.

## Conclusions

Point is, syntactic tricks are not type safety (a reference to [Names are not type safety](https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety)).

If you liked this post and appreciate the effort, consider becoming a [sponsor](https://github.com/sponsors/effectfully-ou) (starts from 1$).
