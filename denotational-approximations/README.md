# Denotational under- and over-approximations

**EDIT** I changed some misleading wording in this post, sorry for being so unclear.

I've recently made a [post about monadic lenses](https://github.com/effectfully/sketches/tree/master/extensible-monadic-lenses) and Edward Kmett reacted with

> So here is an admittedly fairly brutal question that I pose to anybody who offers me "monadic lenses":

> What, if any, are your laws? i.e. What tools can you offer users to reason about these things?

so I want to talk about laws and reasoning in general.

What are laws all about in Haskell? Equality.

> The best way to start a fight in a room full of type theorists is to bring up the topic of equality. -- Conor McBride

A law is usually of the form `x = y` for some notion and denotation of `=` (sometimes people write `=`, sometimes `~`, or `==`, or `~~`, or `===` (I prefer that one or unicode symbols like `≡` or `≈`), or `~~~`). The thing is that `=` is not defined rigorously anywhere. E.g. the `transformers` package defines these laws

```haskell
lift . return = return
lift (m >>= f) = lift m >>= (lift . f)
```

and the documentation for `pipes` states

```haskell
-- The monad transformer laws are correct when viewed through the observe function:

observe (lift (return r)) = observe (return r)
observe (lift (m >>= f)) = observe (lift m >>= lift . f)
```

So the `pipes` package defines its own equality and `Pipe` is a quotient type actually.

Everything related to bottoms and `seq` is completely ignored by that `=`. See [Hask is not a category](http://math.andrej.com/2016/08/06/hask-is-not-a-category/) and [State monads don't respect the monad laws in Haskell](https://mail.haskell.org/pipermail/haskell/2002-May/009622.html). 

Okay, so `=` is a rather vague beast. But besides being vague, its presentation also does some actual harm, because equalities usually hold denotationally (for some definition of "hold") and operational semantics (which is very important in the programming setting) is not even considered, which can lead to true bugs. Here is an example: by default `(*>)` is defined in terms of `(<*>)` like this: `a1 *> a2 = (id <$ a1) <*> a2` and `f <*> a` is very often strict and defined as "compute `f`, compute `a`, apply the result of the former computation to the result of the latter". Now if something fails to inline you get that `a1 *> a2` has this semantics: "compute `a1`, compute `a2`, discard the result of the former computation, return the result of the latter". "Sounds fine" you might think, but if `a2` is a recursive call, the computation won't be tail-recursive and you'll get a space leak. This is not some imaginary situation: I actually got a space leak, because `(*>)` wasn't specified for `Control.Monad.Trans.State.Strict` in the beginning of 2017. So I dumped Core and saw how a perfectly tail-recursive function compiles to something like

```haskell
f 0 = return 1
f x = f (x - 1) >>= return
```

which builds this grotesque thing in memory: `((f (x - 3) >>= return) >>= return) >>= return`. Yes, `m >>= return` and `m` differ operationally despite being denotationally the same thing. Even though GHC tries hard to be smart and optimize as much as possible, rewrite rules and inlining are not a panacea and optimizations can interfere with each other. That flaw can be fixed by presenting laws in a way like "`m >>= return === m`, but prefer the latter for efficiency reasons".

The fact that a definition obeys some laws doesn't make it automatically well-behaved. `monad-control` has laws. Did it help? [No](http://blog.ezyang.com/2012/01/monadbasecontrol-is-unsound/).

Speaking generally, **it is obviously good to have laws**, but it is not obviously bad not to have laws in case they weren't found for a particular abstraction (`Foldable` doesn't have clear laws and it's in `base`). Speaking less generally, laws for pure lenses are very important, because

> On the other hand, I'm not willing to give up on `PutPut` as it forms the backbone of reasoning about code written with lens and determining the canonical nature of the combinators we supply. -- Edward Kmett

but I'm not yet convinced that supposed laws for monadic lenses are of the same importance, so I do not agree currently with the following sentiment:

> the above situation [no laws for monadic lenses] ultimately killed any expression of a "monadic lens."
