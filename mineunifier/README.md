# Teaching GHC how to play Minesweeper

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

## The algorithm

In order to find a solution for the given board we're going to create a set of equations and let GHC solve it. For that we'll turn each cell on the board into a `Constraint` representing a possibly empty set of equations. In order to produce this set we'll need to look not only at the cell, but also at its neighbours, which suggests the following definition:

```haskell
type family NeighbsToRules (c :: Cell) (nbs :: [Cell]) :: Constraint where
    NeighbsToRules 'X     _           = ()                         -- [1]
    NeighbsToRules ('N n) '[]         = n ~ 'Z                     -- [2]
    NeighbsToRules ('N n) (nb ': nbs) = NeighbsToRulesGo n nb nbs  -- [3]
```

[1] says that if we're looking at a bomb, we can't get any info about the neighbours at this point, i.e. we can't tell if they should be bombs or numbers.

[2] says that if we're looking at a number and there are no adjacent cells, then this number must be zero, i.e. there's no bombs around. This handles the case when the entire row contains only one cell that happens to be a number (either by being a number in the input or by GHC figuring out it should be a number using the algorithm we're currently devising).

[3] says that if we're looking at a number and there's a non-zero amount of adjacent cells, then we defer to the `NeighbsToRulesGo` type class taking three arguments and recursing on the last of them:

```haskell
class NeighbsToRulesGo (n :: Peano) (nb :: Cell) (nbs :: [Cell])
instance Rule n nb 'Z => NeighbsToRulesGo n nb '[]
instance (Rule n nb n', NeighbsToRulesGo n' nb' nbs) => NeighbsToRulesGo n nb (nb' ': nbs)
```

- the first argument of the type class is the number of remaining bombs in all the remaining neighbours of the original cell `c`
- the second argument is the next neighbour
- the third argument is all other remaining neighbours

Initially `NeighbsToRulesGo` is instantiated in the definition of `NeighbsToRules` with:

- the number content of the original cell `c`
- its first neighbour
- a list of all the other neighbours

At each recursive step we create a `Rule n nb n'` constraint, which takes three arguments:

- the number of remaining bombs
- a cell adjacent to `c`
- the new number of remaining bombs after accounting for the cell from the previous bullet

This is the central idea. At each step we either decrement the number of bombs remaining in cells adjacent to `c` (if the neighbour at point is a mine) or keep it the same (if the neighbour at point is a number) and put the result into a new type variable `n'`. We recurse like that on the neighbours of `c` creating a new type variable at each step and relating it to the previous one via the `Rule` type class. By the time we reach the end of the list, i.e. run out of neighbours of `c`, we know that the number of remaining bombs has to be zero, which is what conveyed by the first instance of `NeighbsToRulesGo`.

`Rule` is declared like this:

```haskell
class Rule (n :: a) (c :: Cell) (p :: a)
```

The behavior that was just described could be implemented via these two instances:

```haskell
instance Rule ('S n) 'X     n  -- Mine, so decrement.
instance Rule n      ('N p) n  -- Number, so keep the same.
```

however this wouldn't allow us to actually solve anything. As with all the equality constraints we used for parsing, we have to structure `Rule` carefully in order to explicitly drive inference. Hence we turn the two instances into

```haskell
instance {-# INCOHERENT #-} n ~ 'S p => Rule n 'X     p
instance {-# INCOHERENT #-} n ~ p    => Rule n ('N m) p
```

Here instead of requiring `n` to unify with `'S p` in the head of the former instance we do that in its context, so that the instance is picked as soon as it's clear that the neighbour at point is a mine, allowing GHC to reify the old/new remaining numbers of mines accordingly. The latter instance works similarly.

Why `INCOHERENT`, I hear you ask? Well, we know that a mine causes the number of remaining mines to get decremented, but the opposite is also true: if we see that the number of remaining mines got decremented, then the neighbour at point has to be a mine. Which we spell in Haskell like this in the base case of new number of remaining being zero:

```haskell
instance {-# INCOHERENT #-} (n' ~ 'Z, c ~ 'X)  => Rule ('S n') c 'Z
```

Similarly, if the number of remaining mines is zero, then no mine can occur in the remaining neighbours:

```haskell
instance {-# INCOHERENT #-} (c ~ 'N m, p ~ 'Z) => Rule 'Z      c p
```

Those two are the base cases of recursion and we also need steps:

```haskell
instance {-# INCOHERENT #-} (p ~ 'S p', Rule ('S n') c p') => Rule ('S ('S n')) c p
instance {-# INCOHERENT #-} (n ~ 'S n', Rule n'      c p') => Rule n            c ('S p')
```

The former says that no cell can decrement the number of remaining mines by more than one mine and the latter says that no cell can increment the number of remaining mines. Both recurse as appropriate.

The instances are marked with `INCOHERENT`, because there are multiple paths that instance resolution can take when resolving a `Rule` constraint and GHC normally refuses to pick one at random, so we give it an explicit indulgence. We don't care which path is taken in the end, because they all are equivalent due to the set of instances being perfectly coherent.

It only remains to invoke `NeighbsToRules` for each cell (and its neighbours) of the board. Doing that for matrices represented as lists of lists is some awkward (try it yourself!) boilerplate which I'll show for completeness but won't bother explaining, since there's nothing type-level-specific about it:

```haskell
type family HeadDef z xs where
    HeadDef z '[]      = z
    HeadDef _ (x ': _) = x

type family MakeRulesRow ss ps cs ns :: Constraint where
    MakeRulesRow _  _  '[] _        = ()
    MakeRulesRow ss ps (c ': cs) ns =
        ( -- This is where we invoke 'NeighbsToRules' for a cell @c@ and a list of its neighbours.
          NeighbsToRules c (Take 3 ss ++ Take 2 ps ++ Take 1 cs ++ Take 2 ns)
        , MakeRulesRow (Take 1 ps ++ '[c] ++ Take 1 ns) (Drop 1 ps) cs (Drop 1 ns)
        )

type family MakeRulesGo ps (css :: [[Cell]]) :: Constraint where
    MakeRulesGo _ '[]          = ()
    MakeRulesGo ps (cs ': css) =
        ( MakeRulesRow '[] ps cs (HeadDef '[] css)
        , MakeRulesGo cs css
        )

type MakeRules result = MakeRulesGo '[] result
```

Note that the order that the neighbours of a cell come in doesn't matter, since all of them are going to be turned into constraints in the end and constraints are inherently unordered.

This concludes the main part of the algorithm.

## Additional mechanics

Clicking on a cell in a regular version of Minesweeper either terminates the game (if the cell contained a mine) or reveals the number of mines in the neighbours of the cell. We'll implement both of these mechanics as the following class:

```haskell
class Reveal (answer :: a) (puzzle :: a)
instance answer ~ 'X   => Reveal answer 'X
instance answer ~ 'N p => Reveal answer ('N p)
instance answer ~ '[]  => Reveal answer '[]
instance (as ~ (a ': as'), Reveal a p, Reveal as' ps') => Reveal as (p ': ps')
```

Here `answer` is a fully elaborated board (each its cell is either a mine or a number) and `puzzle` is a board that we task GHC to solve (some cells may be unknown, i.e. parsed from `"?"`). We use `answer` to check that GHC solved `puzzle` correctly.

Semantically, `Reveal answer puzzle` is the same thing as `answer ~ puzzle` (a simple proof by induction), however operationally it's a very different thing. Note that we always match on `puzzle`, thus if GHC doesn't know if a cell is a mine or a number, it won't pick any instance until it figures out what the cell is (which may never happen, eventually resulting in a type error). With `answer ~ puzzle` GHC would be able to use `answer` to solve `puzzle`, which would defeat the whole purpose of the game -- we want GHC to play it without cheating by looking at the answer!

In other words, a `Reveal answer puzzle` constraint ensures that the puzzle is solved correctly without revealing the answer to GHC prematurely. For example if GHC determines that some cell is a number, but it's in fact a mine, then we get an error when this instance is picked:

```haskell
instance answer ~ 'N p => Reveal answer ('N p)
```

since the expected `answer` (a mine, i.e. an `'X`) doesn't unify with an `'N`.

We do however want to reveal parts of the answer as GHC advances through the board, namely the exact number content of each cell that GHC has determined to be some number and definitely not a mine. This behavior is implemented by the instance for `'N` above:

```haskell
instance answer ~ 'N p => Reveal answer ('N p)
```

Note how we don't match on `p` iteratively in this instance to conceal `answer` until all of `p` is known replicating the previously described logic of `Reveal`. Instead we specify that as long as it's known that the content of a cell is some `'N p`, it's fine to look up into the `answer` to get the value of `p` if it's not known (and check it if it's known).

Finally, when you play some regular version of Minesweeper, you're given an exact number of mines to allocate on the board, so we need to replicate this functionality too. Counting the number of mines is trivial:

```haskell
type family CountXs (a :: k) :: Nat where
    CountXs 'X        = 1
    CountXs (y ': ys) = CountXs y + CountXs ys
    CountXs _         = 0
```

It only remains to reveal to GHC that the number of mines in the `puzzle` is equal to the number of mines in the `answer`, which we can do by putting the latter into a spurious cell that is adjacent to all cells in the `puzzle`.

Combining `Reveal` with this trick gives us the final definition of the `Verify` type class that checks solutions figured out by GHC without revealing the expected answer fully (while revealing those parts of it that are supposed to be revealed):

```haskell
class Verify (answer :: [[Cell]]) (puzzle :: [[Cell]])
instance
    ( Reveal answer puzzle
    , NeighbsToRules ('N (FromNat (CountXs answer))) (Concat puzzle)
    ) => Verify answer puzzle
```

## Tests

Next we'll add a type class for tests:

```haskell
class Game (number :: Nat) where
    type family ToSolve number :: [[Symbol]]
    type family ToCheck number :: [[Cell]] -> Constraint
```

A `Game` is a pair of two type families: `ToSolve` is for accessing the puzzle and `ToCheck` is for verifying that the puzzle is solved correctly by GHC. The `number` argument is so that we can have multiple tests and distinguish between them using only a type-level number.

We could've made `ToCheck` return a `[[Symbol]]` like `ToSolve` does, but that would make it possible to accidentally let GHC look into the solution in some inappropriate way (i.e. not via the carefully setup logic of `Verify`), so we don't expose the fully elaborated board from the `Game` type class and instead insist on hardcording the logic of `Verify` right into every implementation of `ToCheck`. We do this via this type class:

```haskell
class Check preanswer puzzle
instance (Parse preanswer answer, Verify answer puzzle) => Check preanswer puzzle
```

It parses a fully elaborated board with string cells (i.e. `preanswer` is of kind `[[Symbol]]`) into a board with parsed cells (i.e. `answer` is of kind `[[Cell]]`) and then verifies that the `puzzle` is solved correctly by GHC using the `Verify` class.

Finally, we need to assemble all pieces together to define a function that takes a type-level number `n` (to get the `n`th game), makes GHC play that game (by resolving all the constraints that the board gets "compiled" to), checks that the solution aligns with the expected answer and renders the solution:

```haskell
play
    :: forall number (result :: [[Cell]]).
       (Parse (ToSolve number) result, MakeRules result, ToCheck number result, DisplayGamey result)
    => IO ()
play = do
    putStrLn "Solution:"
    putStrLn $ displayGamey @result
```

Our zeroth example is an empty board, i.e. a board with no rows at all:

```haskell
instance Game 0 where
    type ToSolve 0 =
        '[ '[]
         ]
    type ToCheck 0 = Check
        '[ '[]
         ]

-- >>> play @0
-- Solution:
```

The comment is a GHCi session. Running `play @0` and not getting any type errors ensures that GHC discharges all the constraints and hence successfully solves the board and checks that the solution is equal to the expected answer. Which is why it's safe to print the `"Solution:"` string before getting to printing the actual solution: if there was no solution, then the code would've failed at compile time, hence if the code type checks, we know there's a solution.

Next come a couple of tests for a board with only one cell (either a mine or not):

```haskell
instance Game 1 where
    type ToSolve 1 =
        '[ '["?"]
         ]
    type ToCheck 1 = Check
        '[ '["0"]
         ]

-- >>> play @1
-- Solution:
-- 0

instance Game 2 where
    type ToSolve 2 =
        '[ '["?"]
         ]
    type ToCheck 2 = Check
        '[ '["x"]
         ]

-- >>> play @2
-- Solution:
-- x
```

GHC is able to solve both by making use of the "additional mechanic" of the total number of mines being known. When there's only one mine on the whole board and the puzzle only has one row with one unknown cell, that cell has to be a mine. Similarly, when there are no mines and only one cell in the puzzle, that cell has to be a number and since there are no adjacent cells, the number is `0`.

Let's now look at an example that fails to type check (ignore the fact that test numbers aren't consecutive):

```haskell
instance Game 6 where
    type ToSolve 6 =
        '[ '["?", "?"]
         ]
    type ToCheck 6 = Check
        '[ '["x", "1"]
         ]

-- >>> play @6
-- <interactive>:262:2: error:
--     • Could not deduce: (NeighbsToRules r0 '[r1],
--                          NeighbsToRules r1 '[r0])
--         arising from a use of ‘play’
--     • In the expression: play @6
--       In an equation for ‘it’: it = play @6
-- <interactive>:262:2: error:
--     • No instance for (Rule n'0 r1 'Z) arising from a use of ‘play’
--     • In the expression: play @6
--       In an equation for ‘it’: it = play @6
```

The `? ?` puzzle can be solved by both `x 1` and `1 x` and since the right solution is hidden inside of `ToCheck`, GHC doesn't know how to solve the inherently ambiguous puzzle and we get a (badly looking) type error. We could've made the type error readable by employing the [Detecting the undetectable](https://blog.csongor.co.uk/report-stuck-families) trick, but who cares anyway.

Let's now look at an actually interesting example:

```haskell
instance Game 10 where
    type ToSolve 10 =
        '[ '["2", "?", "2"]
         , '["?", "?", "?"]
         , '["?", "?", "3"]
         ]
    type ToCheck 10 = Check
        '[ '["2", "3", "2"]
         , '["x", "x", "x"]
         , '["x", "x", "3"]
         ]

-- >>> play @10
-- Solution:
-- 2 3 2
-- x x x
-- x x 3
```

There's only one way to solve the puzzle: by first turning

```
2 ? 2
? ? ?
? ? 3
```

into


```
2 ? 2
? x x
? x 3
```

because both the `2`s have three unknown neighbours and so we can't use them to determine any of the neighbours and the only remaining known cell is `3` in the bottom right corner. That cell has exactly three neighbours, so we know all of them are mines.

With the next step we get to

```
2 3 2
? x x
? x 3
```

because the only cell that we can use is the top right one having two mines around it, both of which are known at this point, hence the unknown cell has to be a number, which GHC is allowed to look up in the answer (by one of the additional mechanics).

```
2 3 2
? x x
? x 3
```

Now we can either use the freshly discovered number `3` to determine its only unknown neighbour or we can determine this cell by using the top left one -- either way we get to

```
2 3 2
x x x
? x 3
```

At which point it only remains to resolve one cell, which we know has to be a mine, since the answer contains 5 mines and that information is available to GHC.

As you can see there's quite a lot of reasoning involved here. We have to search for suitable cells, then come back to previously encountered ones and use those once enough information is unlocked elsewhere etc. Compiling to constraints saves us from the burden of implementing any of that logic. The power of declarative programming!

As a final example, GHC is able to solve this somewhat sizeable puzzle:

```haskell
instance Game 18 where
    type ToSolve 18 =
        '[ '["?", "?", "?", "1", "0", "0", "0"]
         , '["?", "?", "?", "2", "0", "1", "1"]
         , '["?", "?", "?", "1", "0", "2", "?"]
         , '["?", "?", "?", "1", "0", "2", "?"]
         , '["?", "?", "?", "1", "1", "1", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         ]
    type ToCheck 18 = Check
        '[ '["0", "1", "x", "1", "0", "0", "0"]
         , '["1", "3", "3", "2", "0", "1", "1"]
         , '["1", "x", "x", "1", "0", "2", "x"]
         , '["1", "2", "2", "1", "0", "2", "x"]
         , '["1", "1", "1", "1", "1", "1", "1"]
         , '["x", "2", "2", "x", "1", "1", "1"]
         , '["2", "x", "2", "1", "1", "1", "x"]
         ]

-- >>> play @18
-- Solution:
-- 0 1 x 1 0 0 0
-- 1 3 3 2 0 1 1
-- 1 x x 1 0 2 x
-- 1 2 2 1 0 2 x
-- 1 1 1 1 1 1 1
-- x 2 2 x 1 1 1
-- 2 x 2 1 1 1 x
```

For more examples see [`Game.hs`](src/Mineunifier/Game.hs)

## Conclusions

GHC allows one to do some pretty crazy stuff at the type level. Also apparently there exist people who read this far for some reason.
