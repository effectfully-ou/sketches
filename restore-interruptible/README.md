# It's not a no-op to unmask an interruptible operation

This post discusses one very subtle gotcha related to asynchronous exceptions and masking. The reader is assumed to be familiar with these concepts. If not, [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell) is a great tutorial.

The gotcha is that this piece of code:

```haskell
mask $ \restore -> do
    putMVar var x
    <...>
```

and this one:

```haskell
mask $ \restore -> do
    restore $ putMVar var x
    <...>
```

are not equivalent, even though one could expect that `restore` unmasking async exceptions and essentially making a computation interruptible would have no effect on an already interruptible operation, which `putMVar` is.

What's an interruprible operation? Quoting Asynchronous Exception Handling in Haskell:

> To quote GHC's documentation:
>
> > Some operations are interruptible, which means that they can receive asynchronous exceptions even in the scope of a mask. Any function which may itself block is defined as interruptible... It is useful to think of mask not as a way to completely prevent asynchronous exceptions, but as a way to switch from asynchronous mode to polling mode.
>
> ```haskell
> mask $ \restore -> do
>   a <- takeMVar m
>   restore (...) `catch` \e -> ...
> ```
>
> If `takeMVar` could not be interrupted, it would be possible for it to block on an `MVar` which has no chance of ever being filled, leading to a deadlock. Instead, GHC's runtime system adds the concept that, within a masked section, some actions can be considered to “poll” and check if there are async exceptions waiting.

And there's one additional piece of information, quoting Parallel and Concurrent Programming (thanks to [Nicolas Frisby](https://github.com/nfrisby) for digging that out):

> An interruptible operation may receive an asynchronous exception only if it actually blocks.

With all of that in mind, let's dissect the behavior of the first snippet:

```haskell
mask $ \restore -> do
    putMVar var x  -- [1]
    <...>          -- [2]
```

There are two scenarios:

1. `putMVar` does not block or temporarily blocks but unblocks before an async exception arrives (if any). The outcome: `var` gets filled with `x`, [1] succeeds, [2] executes
2. `putMVar` blocks (due to already being filled with, say, `y`) and an async exception arrives. The outcome: `putMVar` is cancelled (due to being an interruprible operation), `var` is left filled with `y`, [1] fails, [2] does not execute

Now if we call `putMVar` within the scope of `restore`:


```haskell
mask $ \restore -> do
    restore $ putMVar var x  -- [1]
    <...>                    -- [2]
```

both of these scenarios can occur as well, however there's also a third one:

3. `putMVar` does not block and finishes (or temporarily blocks but unblocks) **right before an async exception arrives within the scope of `restore`**. That causes `putMVar` to succeed but the whole block enclosed in `restore` to fail. The outcome: `var` is filled with `x`, [1] fails, [2] does not execute

This is a completely new scenario: previously [2] would execute iff `putMVar` succeeded, but now [2] can fail to execute even in case of `putMVar` succeeding, which can break an invariant in your code and cause a deadlock (which is how I discovered this subtlety when I was implementing tests for the [Prerun an action](https://github.com/effectfully-ou/haskell-challenges/tree/master/h5-prerun-action) challenge)

So what happens is that `putMVar` does not fill the entire "exception-sensitive box" that `restore` creates within its scope. There's that tiny space between the internal `putMVar` and the enclosing `restore` where an async exception can be raised. So beware.

If you liked this post and appreciate the effort, consider becoming a [sponsor](https://github.com/sponsors/effectfully-ou) (starts from 1$).
