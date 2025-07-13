Imagine having a programming language, wouldn't it be nice to somehow allow for proving
properties about programs written in it? Alas, most languages do not support any kind of formal
reasoning, so the only option is to model the language in a proof assistant and do the proving
there. But what does that mean, exactly?

On the other hand, it's extremely common for programming languages to get compiled down to a
Core language as an intermediate stop on the way to machine code, since a Core language is more
suitable for optimization and analysis. Wouldn't it be nice to prove that compilation is
correct for some definition of "correct"?

And finally, wouldn't it be nice to somehow compose some random proof about a program written
in the high-level language with the proof of the compiler being correct to claim that
compilation preserves the holdability of the proof about the high-level program, whatever that
means?

This document is a typecheckable Agda file that attempts to clarify the ideas that were just
described. It doesn't really provide any answers, just elaborates on the questions.

We're going to look at a simple example right away, but first some preliminaries:

```agda
module README where

open import Function
open import Relation.Binary.PropositionalEquality
open import Data.Bool
```

Types of the target language. These are the types available for us in both the high-level
and the core languages:

```agda
infixr 4 _⇒_
data Type : Set where
  bool : Type
  _⇒_  : Type -> Type -> Type
```

Interpretation of target-languages types in the source language (i.e. Agda).
We choose the type language of Agda as our semantic domain for simplicity of the presentation.
In any actual development we'd have to use something else for more control over
evaluation semantics and whatnot (in particular, Agda is lazy and the target languages are
likely not), more on that later.

```agda
⟦_⟧ᵗ : Type -> Set
⟦ bool  ⟧ᵗ = Bool
⟦ σ ⇒ τ ⟧ᵗ = ⟦ σ ⟧ᵗ -> ⟦ τ ⟧ᵗ
```

Let's pretend we have these (we're pretending not because it's hard to implement all the things
in this `postulate` block, but for brevity, all the definitions are trivial):

```agda
postulate
  -- Type-indexed representation of STLC with booleans and let-expressions.
  -- This is our "high-level" language.
  Termʰ : Type -> Set

  -- Type-indexed representation of STLC with booleans but without let-expressions.
  -- This is our core language.
  Termᶜ : Type -> Set

  -- A compiler that turns the AST of the high-level language into the AST of the low-level
  -- language by turning every let-expression into a function application.
  compile : ∀ {σ} -> Termʰ σ -> Termᶜ σ

  -- Interpretation of high-level terms as Agda expressions.
  ⟦_⟧ʰ : ∀ {σ} -> Termʰ σ -> ⟦ σ ⟧ᵗ

  -- Interpretation of low-level terms as Agda expressions.
  ⟦_⟧ᶜ : ∀ {σ} -> Termᶜ σ -> ⟦ σ ⟧ᵗ
```

Now to the actually hard stuff:

```agda
postulate
  -- A proof that a high-level term means the same thing as the low-level term that we get by
  -- compiling the former. For our language with lambdas that means we'd have to use functional
  -- extentionality if we were to actually prove this statement.
  compilation-soundness : ∀ {σ} -> (t : Termʰ σ) -> ⟦ t ⟧ʰ ≡ ⟦ compile t ⟧ᶜ
```

Now we can show how to compose a proof about the compiler and a proof about the meaning of a
program written in the high-level language.

The prerequisites:

```agda
postulate
  -- A high-level term representing we don't care what.
  ex : Termʰ ((bool ⇒ bool) ⇒ bool ⇒ bool)

  -- Some random property.
  WhateverProperty : ∀ {σ} -> ⟦ σ ⟧ᵗ -> Set

  -- A proof that `WhateverProperty` holds for the meaning of the high-level term.
  whateverProof : WhateverProperty ⟦ ex ⟧ʰ
```

We can "compose" a proof that some random property holds for the meaning of a high-level term
and a proof that the compiler is sound to get a proof that the property holds for the meaning
of the low-level term that we get by compiling the high-level one.

```agda
whateverProofTranslated : WhateverProperty ⟦ compile ex ⟧ᶜ
whateverProofTranslated = subst WhateverProperty (compilation-soundness ex) whateverProof
```

Is this it then? Can we just automatically translate proofs about terms of the high-level
language to get proofs about terms of the low-level language?

No, and for multiple reasons.

First of all, any real development of this kind wouldn't use Agda's type space as a semantic
domain. Instead it would be a dedicated data type for representing the meaning of a
high/low-level term. And equality wouldn't be `_≡_`, it would be observational equality or
bisimulation or contextual equality or whatever. The problem then is that we can't call
`subst` over a proof of some arbitrary user-defined equality relation, `subst` only works
over `_≡_`. I.e. the soundness proof about the compiler can only be composed with a proof of
some random property if the property respects the equality relation that the soundness proof
uses. For example, if the semantic domain is untyped lambda calculus and the equality relation
is "the two sides evaluate to the same value", then it's only possible to carry a proof about
a high-level term to its low-level counterpart when that proof is about something
evaluation-related, exclusively. In particular, well-typedness of a high-level term doesn't
necessarily translate to its compiled form, if all we know about the compiler is that
compilation preserves some evaluation semantics.

So naturally enough, what we can get out of a soundness proof about the compiler depends on
how strong our notion of "soundness" is and how well that fits into the framework used for
proving arbitrary properties about programs written in the high-level language. And that
framework can be a completely different proof system than the one used for providing the
soundness proof of the compiler. Composing a custom equality proof with a proof of some
random property constructed in a completely different system, possibly even relying on
different mathematical foundations can't be done in any automatic way and requires lots and
lots of very careful reasoning about one arbitrary complex proof system fitting into another
one.

However our problems only start here, even under perfect circumstances where all the proofs are
within the same system and the equality relation of the soundness proof aligns well with the
equality relation used by the system (like how we had `_≡_` above or, for another example,
when the soundness proof uses observational equality and the proof system is based on it as
well).

I kept saying that the `WhateverProperty` proofs were about the meaning of a term, not a term
itself. It doesn't make sense to talk about translating proofs about terms, because if
`WhateverProperty` was defined over `Termʰ`, it would fail to translate to `Termᶜ` right away:
even though we could align the types by lifting the `Termᶜ` that we get after compilation back
into a `Termʰ`, the original property could be something like "this term has a non-zero amount
of let-expressions in it", which wouldn't be true after compilation anymore. I.e. compilation
cannot preserve arbitrary properties of a term, only of its meaning (which itself can only
happen under certain circumstances as was discussed earlier).

The practical implication of this is that the person proving an arbitrary property has to deal
with both the AST of the high-level language and its interpretation in the meta language used
for proving. To say it differently, one can't just write some arbitrary piece of Agda code and
claim that it proves some property about a program written in the high-level language -- the
connection between the Agda proof and the high-level program has to be established. One way of
doing that is by making the person writing the proof construct an actual AST, i.e. a value of
type `Termʰ`, and then prove things about its meaning. But it's clearly not what people would
like to do instead of just writing Agda code directly and proving properties about the Agda
code without dealing with the AST of the high-level language explicitly.

So what we could do instead is to have some kind of automatic extraction of high-level
language code from Agda code. With our example definitions the extraction function could look
something like this:

```agda
postulate
  extract : ∀ {σ} -> ⟦ σ ⟧ᵗ -> Termʰ σ
```

(which is probably actually implementable for our very limited language, see this for an
example: https://github.com/effectfully/STLC/blob/3826d7555f37d032d07cf414c7f6770fc32918f1/src/STLC/NbE/Read.agda#L29)

Then we'd need to prove that extraction is sound:

```agda
postulate
  extraction-soundness : ∀ {σ} -> (x : ⟦ σ ⟧ᵗ) -> x ≡ ⟦ extract x ⟧ʰ
```

which together with compilation soundness gives us our best possible result:

```agda
full-soundness : ∀ {σ} -> (x : ⟦ σ ⟧ᵗ) -> x ≡ ⟦ compile (extract x) ⟧ᶜ
full-soundness x = trans (extraction-soundness x) (compilation-soundness (extract x))
```

That reads as: compilation preserves the meaning of a program extracted from an Agda value.

Note that we can use `extract` to, well, extract an actual program in the high-level language
and use it the way the high-level is supposed to be used, with all the proofs being valid
and without the need to write the program twice: once in Agda for proofs and once in the
high-level language itself.

Having all this we can translate a proof of an arbitrary property about *any* value of type
`⟦ ty ⟧ᵗ` (previously that value had to be of the `⟦ term ⟧ʰ` form):

```agda
postulate
  ty  : Type
  val : ⟦ ty ⟧ᵗ
  whateverProofNoEval : WhateverProperty val

whateverProofNoEvalTranslated : WhateverProperty ⟦ compile (extract val) ⟧ᶜ
whateverProofNoEvalTranslated = subst WhateverProperty (full-soundness val) whateverProofNoEval
```

I.e. the person doing the proving no longer needs to worry about the AST of the high-level
language and the translation of proofs works automatically as long as the type of the value
that the proof is talking about is an interpretation of some target-language type. I.e. there's
still a need to make sure that proofs are about things whose types are compatible with the
type system of the target languages -- but once this is ensured, we get the rest for free.

However, this is only the best case scenario and for any practical system it would not be
possible to define `extract` as a regular function (because type-driven quotation only works
for trivial systems: add natural numbers and you no longer can get the first-order
representation of a function just by looking at its type). It doesn't have to be a regular
function though! It can be a macro or something similar based on reflection. Agda has such
capabilities, but of course proving soundness of a function is fundamentally simpler than
proving soundness of a macro (whatever that even means).

Summarizing, the whole pipeline looks like this:

```code
       Agda to Termʰ      Termʰ to Termᶜ               Termᶜ to Agda
⟦term⟧ ------------> term -------------> compiled-term ------------> ⟦compiled-term⟧
        extraction         compilation                  evaluation
```

where `⟦term⟧` is a name denoting an Agda value from which a `term : Termʰ` can be extracted.

This pipeline allows us to get the Agda interpretation of a compiled program extracted from
some Agda value. If `compilation-soundness` and `extraction-soundness` both hold, then
`⟦term⟧` and `⟦compiled-term⟧` are equal and every proof about `⟦term⟧` automatically
translates to `⟦compiled-term⟧`.

So the person doing the proving writes `⟦term⟧` (a regular Agda value) and can get their
hands on `⟦compiled-term⟧` (which is also a regular Agda value) without ever needing to
explicitly go via the AST of either of the target languages -- that happens behind the scenes.
Apart from still needing to worry about types when crossing language boundaries (which is
always the case when going from one typed language to another).

Unfortunately that types part is way more complicated than it may seem. But that's a story for another day.
