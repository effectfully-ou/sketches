# Hierarchical Free Monads: Mostly Pointless
# _**(And The Resurrection Of Final Tagless)**_

This is a response to [Hierarchical Free Monads: The Most Developed Approach In Haskell](https://github.com/graninas/hierarchical-free-monads-the-most-developed-approach-in-haskell/blob/master/README.md). The article makes some bold claims, but it builds on an initial straw man argument that with FT one has to enumerate all individual effects of a function in its type signature, which is not true.

### The straw man argument

Here is one quote:

> Although Free Monads require some work on the language definition and implementation levels, this approach doesn't bring extra boilerplate into the business logic layer. This is good because boilerplate in the business logic costs much more. Let's compare this for FT and HFM.
>
> Simple scenario in this small HFM framework:
>
> ```haskell
> printRandomFactorial :: App ()
> printRandomFactorial = do
>     n <- getRandomInt (1, 100)
>     logInfo $ show $ fact n
> ```
>
> The same scenario with FT:
>
> ```haskell
> printRandomFactorial :: (Random m, Logger m) => m ()
> printRandomFactorial = do
>     n <- getRandomInt (1, 100)
>     logInfo $ show $ fact n
> ```
>
> While the bodies of the two functions are pretty much the same there is a significant difference in the function definitions. FT requires you to specify a list of constraints. The more effects you have the more constraints will be there. Normally, business logic of a regular web service consists of dozens if not hundreds functions, and typing this kind of boilerplate makes coding extremely annoying. It doesn't buy anything useful.

Here is another one:

> _True, FT is easy to extend, but by what cost? Let's see. This is how we can add an effect in FT:_
>
> ```haskell
> -- Then:
> -- printRandomFactorial :: (Random m, WithLog SomeLogEnvironment String m) => m ()
>
> -- Now:
> printRandomFactorial :: (Random m, Database m, WithLog SomeLogEnvironment String m) => m ()
> ```
>
> However once it's done, a lot of code should be updated. If the outer function had a call to printRandomFactorial, it's affected now. And all other functions up to the top of the call stack.

And here is how FT (see the [FT](src/FT.hs) module) can be used in the same manner as HFM:

```haskell
import           System.Random

data LogLevel = Info
type Message  = String

class Monad m => MonadLogger m where
    logMessage :: LogLevel -> Message -> m ()

class MonadLogger m => MonadApp m where
    getRandomInt :: (Int, Int) -> m Int

instance MonadLogger IO where
    logMessage _ = putStrLn

instance MonadApp IO where
    getRandomInt = randomRIO

runAsApp :: IO a -> IO a
runAsApp = id

-- Corresponds to the original @logInfo :: Message -> App ()@
logInfo :: MonadApp m => Message -> m ()
logInfo = logMessage Info

printRandomFactorial :: MonadApp m => m ()
printRandomFactorial = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
```

This snippet achieves the same as the original HFM one (see the [HFM](src/HFM.hs) module), but is nearly twice as short. Just look at that monster:

<details>
  <summary> Monster </summary>
  <p>

```
import           Control.Monad.Free
import           System.Random

data LogLevel = Info
type Message  = String

-- Algebra (interface) for the LoggerL Free monadic language with only 1 method
data LoggerF next where
  LogMessage :: LogLevel -> Message -> (() -> next) -> LoggerF next

-- Functor instance needed for the Free machinery
instance Functor LoggerF where
  fmap f (LogMessage lvl msg next) = LogMessage lvl msg (f . next)

-- Free monadic language
type Logger a = Free LoggerF a

data AppF next where
  GetRandomInt :: (Int, Int) -> (Int -> next) -> AppF next
  EvalLogger :: Logger () -> (() -> next) -> AppF next

instance Functor AppF where
  fmap f (GetRandomInt range next) = GetRandomInt range (f . next)
  fmap f (EvalLogger logAct next)  = EvalLogger logAct (f . next)

type App a = Free AppF a

-- Simple console logger
interpretLoggerF :: LoggerF a -> IO a
interpretLoggerF (LogMessage lvl msg next) = do
  putStrLn msg
  pure $ next ()

runLogger :: Logger a -> IO a
runLogger = foldFree interpretLoggerF

-- Interpreting function
interpretAppF :: AppF a -> IO a
interpretAppF (EvalLogger loggerAct next) = next <$> runLogger loggerAct
interpretAppF (GetRandomInt range next)   = next <$> randomRIO range

-- Interpreter entry point
runApp :: App a -> IO a
runApp = foldFree interpretAppF

-- Log message with Info level.
logInfo :: Message -> App ()
logInfo msg = evalLogger (logMessage Info msg)

-- Helper function to wrap LoggerF method
logMessage :: LogLevel -> Message -> Logger ()
logMessage lvl msg = liftF $ LogMessage lvl msg id

-- Helper function to wrap AppF method
evalLogger :: Logger () -> App ()
evalLogger logger = liftF $ EvalLogger logger id

getRandomInt :: (Int, Int) -> App Int
getRandomInt range = liftF $ GetRandomInt range id

printRandomFactorial :: App ()
printRandomFactorial = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
```
</p></details>

### Redundancy

Moreover, with FT you can use `logMessage` within both `Logger` and `App`, while with HFM you have to either provide two versions of this function or explicitly lift `Logger` actions into `App` in client code. The more subsystems you have, the more bloated your API becomes or the more boilerplate is required in client code (which can be automated with type classes, but then why not use FT in the first place). This setup penalizes modularity, which is no good for an effect system (and HFM is an effect system, just an intentionally weak one).

For example, with a project structured this way:

```
     App
  /   |   \
Baz  Quux  Quuz
  \  /     /
   Foo  Bar
    \   /
    Logger
```

you'll have to define 7 logging functions or to use explicit lifts at the call site.

## Extensibility

Here's another point that the author makes:

> _"With FT you have freedom. You can incorporate an effect whenever you want to do it. You're not limited by someone's opinionated design. It's so easy, and so cool to compose effects. The HFM approach prohibits that. It's too rigid. All the effects should be specified in the algebra. Developer of this framework makes decisions instead of you. He limits your creativity. Are you really suggesting this? Are you really suggesting to restrict the freedom?"_
>
> Yes, I'm really suggesting this. There are several reasons.
>
> - Freedom is not free. Allowing business logic developers to incorporate arbitrary effects into arbitrary places will lead to a mess. Trusting to your developers is a good idea, but providing a unified design for the whole code is the practice one cannot refuse. Freedom of FT is too risky. Being opinionated is not bad, it's a way to decrease risks and to make the development cheaper. This is what HFM does and this is why it's very practical.

While I sympathize with that "freedom is not free" sentiment, there always has to exist an escape hatch. When there's a looming deadline and I want to add an effect to some function just to check if a hypothesis plays out, I don't really care about your cleverly designed hierarchical subsystems, I want to get shit done and move on.

The author seems to dislike the very possibility of extending functions with new effects when those effects are not embedded into some subsystem beforehand:

> Besides that, throwing in new effects is still possible:
>
> ```haskell
> printFactAndFib :: (Lang m, MonadIO m) => m ()
> ```
>
> Compare these movements with a simple App from the Free monadic language:
>
> ```haskell
> printRandomFactorial :: App ()
> printRandomFibonacci :: App ()
> printFactAndFib :: App ()
> ```
>
> This code is very simple. Adding new effects subsystems into the language itself won't break any code. There is no overengineering in the form of smart tricks on the type level. This is why the HFM approach is better for designing software: it keeps accidental complexity low which is orders of magnitude more important then extensibility.

And then "in case you truly need some extensibility", the author proposes to embed the entirety of `IO` into `App`. Which is like the opposite of what "extensibility" is.

Anyway, we can recover the non-extensibility with FT (the [`HFT`](src/HFT.hs) module) while still keeping the whole thing more ergonomic than what HFM has to offer.

First, some library-level boilerplate:

```haskell
newtype Subsystem constr a = Subsystem
    { unSubsystem :: forall m. constr m => m a
    }

instance (forall f. constr f => Functor f) => Functor (Subsystem constr) where <...>
instance (forall f. constr f => Applicative f) => Applicative (Subsystem constr) where <...>
instance (forall m. constr m => Monad m) => Monad (Subsystem constr) where <...>

subsystem :: (forall m. constr2 m => constr1 m) => Subsystem constr1 a -> Subsystem constr2 a
subsystem (Subsystem a) = Subsystem a
```

Here we wrap all the monad parameterization business with the `Subsystem` `newtype`, so that the user can instantiate `Subsystem` with a constraint and get a mere type as a result:

```haskell
type Logger = Subsystem MonadLogger
type App    = Subsystem MonadApp
```

The instances above can be read as "Any subsystem is a `Monad` as long as it's determined by a constraint that subsumes `Monad`".

`subsystem` allows to embed a, well, subsystem into any supersystem, possibly through another subsystem.

Application-level boilerplate looks like this (`deriving newtype` doesn't seem to work with `QuantifiedConstraints`, otherwise there would be less boilerplate):

```haskell
-- Any subsystem has access to a logger as long as it's determined by a constraint that subsumes 'MonadLogger'.
instance (forall m. constr m => MonadLogger m) => MonadLogger (Subsystem constr) where
    logMessage level msg = Subsystem $ logMessage level msg

instance (forall m. constr m => MonadApp m) => MonadApp (Subsystem constr) where
    getRandomInt range = Subsystem $ getRandomInt range

runApp :: App a -> IO a
runApp = unSubsystem
```

Example usage:

```haskell
logged :: Logger ()
logged = logMessage Info "a"

printRandomFactorial' :: App ()
printRandomFactorial' = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
    logMessage Info "b"              -- A 'Logger' action implicitly lifted into 'App'.
    subsystem logged                 -- A 'Logger' action explicitly lifted into 'App'.
```

With this setup `logMessage` can be called within any subsystem. Subsystems can be lifted transitively and only one function is required in all the cases: `subsystem`.

Note that even though the underlying machinery uses advanced features like `QuantifiedConstraints`, the user code is basically the same as with `HFM`, except there's no need to compulsively lift effects. And `Free` is not a completely trivial concept either.

Finally, this non-extensible setup is fully interoperable with the original extensible FT:

- `App` implements `MonadApp`, so the function that allows to go from `forall m. MonadApp m => m a` to `App a` is `id`
- going the other way around is a matter of calling `unSubsystem`, which is also zero-cost

And FT itself is fully interoperable with the rest of Hackage.

### Testability

The [Testability](https://github.com/graninas/hierarchical-free-monads-the-most-developed-approach-in-haskell/blob/master/README.md#testability) section of the article claims that it's more convenient to write tests with free monads than with FT, which is a statement that I agree with. But then it proceeds with an example that can be written with FT just as well. Free monads are particularly handy when you want to implement logic like "if a call to `X` is followed by a call to `Y`, then ...". Handling such logic with FT is messy. But you can always turn an FT computation into an explicit AST and analyze it whatever way you want, without the `Free` monad indirection and the associated boilerplate. Even if you need an explicit AST (which is not going to happen often), it's still more convenient to use FT in actual business logic and reify FT as an AST in tests.

### Conclusions

Thereby I conclude that Hierarchical Free Monads are mostly pointless.
