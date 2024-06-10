# type-safe-eDSL-frontend

## The idea

Ever wanted nicer syntax for your eDSL? And static checks for it such as ensuring well-scopedness? This post is for you.

We're going to pick untyped lambda calculus as our eDSL just to assert superiority, but the trick works perfectly well for an imperative eDSL too and has been tested in that context.

So this is the language:

```haskell
data Term
    = Var String
    | Lam String Term
    | App Term Term
    deriving (Show)
```

(don't take this as an endorsement of using `String`s for variables, this is the worst option of them [all](https://www.schoolofhaskell.com/user/edwardk/bound). Also, if you have an opinion on what the best option is, please let me know)

How would one create a term for such a language? One option is just to write out the AST as it is, taking the O combinator (a.k.a. `owl`) as an example (since it's objectively the coolest combinator)

```
\f g -> g (f g)
```

it'll be

```haskell
Lam "f" $ Lam "g" $ App (Var "g") (App (Var "f") (Var "g"))
```

Quite clunky and noisy with all those parens and quotes.

Another option is to use HOAS to get something like this (given appropriate definitions for `lam` and `app`):

```haskell
lam "f" $ \f -> lam "g" $ \g -> app g (app f g)
```

This is nicer, however it has a number of drawbacks:

- When using HOAS one has to actually bind a Haskell variable whenever they want to use a variable of the target language, which can be a hassle in cases when it's enough to merely know that the target language variable does exist in the current scope. This means that constructing terms with free variables in them requires carrying an environment around as you have to look up variable names to get their Haskell counterparts. This is particularly bad for imperative eDSL, for example if you have a global target language variable that all procedures can read from / write to, then with HOAS you'd have to carry it around in all functions as an argument.
- Having to use Haskell names with HOAS necessarily entails having to avoid clashing with names of common Haskell functions. It may be annoying to force the user to avoid shadowing names like `sum`, `null`, `and`, `id`, `map`, `last` etc or hide them from the implicitly imported `Prelude`.
- Requiring the user to bind each name twice (once as an argument to `lam` such as `"f"` and once as the actual Haskell variable such as `f`) pretty much guarantees that the names will go out of sync somewhere at some point

This post proposes to abuse the `OverloadedRecordDot` extension to get another alternative:

```haskell
lam.f $ lam.g $ app var.g (app var.f var.g)
```

which elaborates to the original AST:

```haskell
Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
```

and as such is basically just a nicer syntax for creating ASTs manually.

The rest of the post is implementing the syntax and a static check for it ensuring well-scopedness of terms.

## Unscoped ([full code]())

Without the well-scopedness check all we need is this:

```haskell
type Prefix :: Symbol -> Type
data Prefix prefix = Prefix

var :: Prefix "var"
var = Prefix

lam :: Prefix "lam"
lam = Prefix

app :: Term -> Term -> Term
app = App

instance (res ~ (Term -> Term), KnownSymbol name) => HasField name (Prefix "lam") res where
    getField _ = Lam $ symbolVal' (proxy# @name)

instance (res ~ Term, KnownSymbol name) => HasField name (Prefix "var") res where
    getField _ = Var $ symbolVal' (proxy# @name)
```

to make the O combinator example from the above work:

```
-- >>> print owl
-- Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
owl :: Term
owl = lam.f $ lam.g $ app var.g (app var.f var.g)
```

And that's it.

## Scoped ([full code](./src/Scoped.hs))

To check well-scopedness we need to somehow only allow those variables to be referenced that were bound before. One way to do that is to make `lam` introduce a contraint required by `var`, for every variable. How do we add a local constraint for an arbirary `Symbol` though? Well, we can simpy make it up using the same way `unsafeCoerce` makes up a constraint between two types:

```haskell
type IsScoped :: Symbol -> ()
type family IsScoped name

type InScope name = IsScoped name ~ '()

data Enter name where
    Enter :: InScope name => Enter name

-- @res ~ (InScope name => Term -> Term)@ doesn't work.
instance (res ~ ((Enter name -> Term) -> Term), KnownSymbol name) =>
        HasField name (Prefix "lam") res where
    getField _ k =
        case unsafeEqualityProof @(IsScoped name) @'() of
            UnsafeRefl -> Lam (symbolVal' (proxy# @name)) $ k Enter

instance (res ~ Term, KnownSymbol name, InScope name) => HasField name (Prefix "var") res where
    getField _ = Var $ symbolVal' (proxy# @name)
```

This is very similar to what we've seen in the previous section, but now we've added scope checking with `IsScoped name ~ '()` being the main idea: we make up this constraint for the body of each lambda and require it to be satisfied for each references variable, so that only bound variables can be referenced.

`Enter` is an artifact of the encoding, it would be nicer if we could simply use the constraint in the `HasField` instance for `"lam"`, but I couldn't make it work (if you can, please let me know).

The O example now becomes

```haskell
-- >>> print owl
-- Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
owl :: Term
owl = lam.f $ \Enter -> lam.g $ \Enter -> app var.g (app var.f var.g)
```

and if we reference a free variable:

```haskell
free :: Term
free = var.x
```

we'll get:

```
error: [GHC-18872]
    • Couldn't match type ‘IsScoped "x"’ with ‘'()’
        arising from selecting the field ‘x’
    • In the expression: var.x
      In an equation for ‘free’: free = var.x
```

which is not very helpful, but can be made a proper error message with the [Detecting the undetectable: custom type errors for stuck type families](https://blog.csongor.co.uk/report-stuck-families/) trick, so that the code above fails type checking with

```
error: [GHC-64725]
    • Can't reference a free variable: ‘x’
    • In the expression: var.x
      In an equation for ‘free’: free = var.x
```

The full code includes such error messages.
