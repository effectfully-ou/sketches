# Making GHC play Minesweeper

Equality constraints are a really powerful feature of GHC. They allow one to

1. [improve](https://chrisdone.com/posts/haskell-constraint-trick) type inference
2. [delegate](https://stackoverflow.com/questions/30224796/how-should-the-general-type-of-a-lemma-function-be-understood?answertab=scoredesc#tab-top) a bunch of mechanical proving to GHC
3. [implement](https://github.com/effectfully-ou/sketches/tree/master/avoid-overlapping-recursive) n-ary functions with good type inference
4. [replace](https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right) functional dependencies increasing expressiveness while preserving type inference
5. [detect](https://github.com/effectfully-ou/sketches/tree/master/poly-type-of-saga/part1-try-unify) and and instantiate polymorphism
6. much more

Today we're going to look at how to express a complex algorithmic problem using equality constraints, so that GHC can solve it for us. Namely, we'll make GHC solve some subset of the deterministic fragment of the [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_(video_game)) game.

**DISCLAIMER**: this is only another showcase of expressiveness of the type system of GHC, written purely for shits and giggles. Don't expect to see anything practical here. There does seem to be a [demand](https://www.reddit.com/r/haskell/comments/w806kf/typesignature_who_wants_to_be_a_millionaire_but/ihmx17l) for this kind of silliness, though.

Huge thanks to [**kana_sama**](https://github.com/kana-sama) for providing test cases. You can also play his version of Minesweeper [online](https://kana-sama.github.io/minecljs).

## Setup

The board in Minesweeper is a matrix of cells. Each cell is one of the following:

1. unopened
2. unopened and marked as potentially containing a mine
3. opened and containing a number representing the number of mines adjacent to the cell

We're going to represent these three states as type-level values of type `Cell`:

```haskell
data Peano = Z | S Peano

data Cell
    = N Peano  -- A cell storing the number of mines adjacent to it
    | X        -- A cell with a mine
```

There are three states but only two constructors of `Cell`. That's because a cell that we don't know anything about we're going to represent as an unresolved type variable of type `Cell`.

`Peano` is a fancy way to spell [`Nat`](https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-TypeLits.html#t:Nat). The name of the data type is [due](https://en.wikipedia.org/wiki/Peano_axioms) to Peano, the mathematician:

> `0` is a natural number.
> For every natural number `n`, `S(n)` is a natural number.

We use `Peano` instead of `Nat`, because the former allows for more ergonomic pattern matching: we'll often need to match against a non-zero number in an instance and with `Peano` it's just matching on `S` while with `Nat` we'd have to jump through quite a few hoops.

Writing `'S ('S ('S 'Z))` instead of `3` is pretty annoying, so we'll need a conversion function from `Nat` to `Peano` and one in the opposite direction:

```haskell
type family FromNat (n :: Nat) :: Peano where
    FromNat 0 = 'Z
    FromNat n = 'S (FromNat (n - 1))

type family ToNat (m :: Peano) :: Nat where
    ToNat 'Z     = 0
    ToNat ('S m) = ToNat m + 1
```

Since we're only worried about a simple deterministic fragment of the game, there's no guessing and `X` always represents a cell with a mine, not just a potential mine.

We'll represent a matrix as a list of lists.

Summarizing, the board is a type-level value of type `[[Cell]]` where some of the `Cell`s can be unresolved type variables. The solver's job is to resolve those type variables to a `Cell` value in [canonical](https://ncatlab.org/nlab/show/canonical+form) form, i.e. only containing constructors of `Cell` and `Peano` and nothing else (no type families, type variables etc).

## IO

Next come "IO" operations.

Boards need to be fed to the solver somehow and writing `'S ('S 'Z)` etc manually is annoying, hence we need a parser. We could parse a raw string, but it's a pain at the type-level and hence instead we're going to parse matrices of strings where each string is the textual representation of a cell. We'll use the same type class for parsing the whole board and its individual parts (rows, cells):

```haskell
class Parse source result
```

Firts the instance that handles unknown cells:

```haskell
instance Parse "?" c
```

It says that if the input cell is `?` then the result stays unconstrained. `c` is going to be instantiated later by the solver (unless the solver fails to find a solution for the board).

If the input is a mine, then so is the output:

```haskell
instance c ~~ 'X => Parse "x" c
```

`~~` is a [heterogeneous](https://ryanglscott.github.io/2021/09/06/leibniz-equality-in-haskell-part-2-heterogeneous-equality) equality constraint. If we used `~`, then instance resolution would only pick this instance for `c` already having kind `Cell`, while we want the instance to be chosen regardless of whether there's anything known about `c`. I.e. it's the same situation as the one described in [The constraint trick for instances](https://chrisdone.com/posts/haskell-constraint-trick) except here the kind level is involved in addition to the type level.

The highest value of a cell storing the number of mines adjacent to it is 8 on a 2D plane, hence we have these 9 instances (the ones in the middle are omitted for brevity):

```haskell
instance c ~~ 'N (FromNat 0) => Parse "0" c
instance c ~~ 'N (FromNat 1) => Parse "1" c
<...>
instance c ~~ 'N (FromNat 8) => Parse "8" c
```

We have lists of cells (a row) and lists of lists of cells (the board) and we handle both of these kinds of lists with only two instances:

```haskell
instance result ~~ '[] => Parse '[] result
instance (Parse s r, Parse ss' rs', rs ~~ (r ': rs')) => Parse (s ': ss') rs
```

The former instance says that if the input is an empty list, then so is the output.

In the latter instance the `Parse s r` and the `Parse ss' rs'` parts are straightforward: they say that in order to parse a list one has to parse the head and the tail of that list. The last constraint is `rs ~~ (r ': rs')` and it says that once both the head and the tail are parsed we can assemble them into the resulting list `rs`.

That concludes parsing. We also need the opposite, pretty-printing. We can get away with a single type class again:

```haskell
class DisplayGamey a where
    displayGamey :: String
```

And the instances are some boring boilerplate:

```haskell
instance DisplayGamey 'X where
    displayGamey = "x"

-- 'KnownNat' allows us to demote a type-level number to the term level, so that we can render it.
instance KnownNat (ToNat m) => DisplayGamey ('N m) where
    displayGamey = show . natVal $ Proxy @(ToNat m)

-- An empty list is rendered as an empty string regardless of the kind of the list.
instance DisplayGamey '[] where
    displayGamey = ""

-- Cells are separated with spaces in a row.
instance (DisplayGamey el, DisplayGamey row) => DisplayGamey (el : row :: [Cell]) where
    displayGamey = displayGamey @el ++ " " ++ displayGamey @row

-- Rows are separated with newlines in the board.
instance (DisplayGamey row, DisplayGamey rows) => DisplayGamey (row : rows :: [[Cell]]) where
    displayGamey = displayGamey @row ++ "\n" ++ displayGamey @rows
```

It only remains to combine parsing and pretty-printing:

```haskell
displayBoard :: forall input result. (Parse input result, DisplayGamey result) => String
displayBoard = displayGamey @result
```

for us to look at an example:

```
>>> :set -XDataKinds
>>> :set -XTypeApplications
>>> putStrLn $ displayBoard @('[ ["1", "1", "0"], ["x", "1", "0"] ])
1 1 0
x 1 0
```

`displayBoard` takes a list of lists of type-level strings, parses it at the type level using the `Parse` type class and pretty-prints the result using the `DisplayGamey` type class.




















class NeighbsToRulesGo (n :: a) (nb :: Cell) (nbs :: [Cell])
instance Rule n nb 'Z => NeighbsToRulesGo n nb '[]
instance (Rule n nb p, NeighbsToRulesGo p nb' nbs) => NeighbsToRulesGo n nb (nb' ': nbs)

type family NeighbsToRules (c :: Cell) (nbs :: [Cell]) :: Constraint where
    NeighbsToRules 'X     _           = ()
    NeighbsToRules ('N n) '[]         = n ~ 'Z
    NeighbsToRules ('N n) (nb ': nbs) = NeighbsToRulesGo n nb nbs




class Rule (n :: a) (c :: Cell) (p :: a)

instance {-# INCOHERENT #-} n ~ 'S p => Rule n 'X     p
instance {-# INCOHERENT #-} n ~ p    => Rule n ('N m) p

instance {-# INCOHERENT #-} (n' ~ 'Z, c ~ 'X)  => Rule ('S n') c 'Z
instance {-# INCOHERENT #-} (c ~ 'N m, p ~ 'Z) => Rule 'Z      c p

instance {-# INCOHERENT #-} (p ~ 'S p', Rule ('S n') c p') => Rule ('S ('S n')) c p
instance {-# INCOHERENT #-} (n ~ 'S n', Rule n'      c p') => Rule n            c ('S p')





class Reveal (answer :: a) (puzzle :: a)
instance answer ~ 'X   => Reveal answer 'X
instance answer ~ 'N p => Reveal answer ('N p)
instance answer ~ '[]  => Reveal answer '[]
instance (as ~ (a ': as'), Reveal a p, Reveal as' ps') => Reveal as (p ': ps')

type family CountXs (a :: k) :: Nat where
    CountXs 'X        = 1
    CountXs (y ': ys) = CountXs y + CountXs ys
    CountXs _         = 0

class Verify (answer :: [[Cell]]) (puzzle :: [[Cell]])
instance
    ( Reveal answer puzzle
    , NeighbsToRules ('N (FromNat (CountXs answer))) (Concat puzzle)
    ) => Verify answer puzzle
