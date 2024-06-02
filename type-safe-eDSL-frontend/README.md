# type-safe-eDSL-frontend

{- Note [Alternative surface syntax implementations]
(P)HOAS can be convenient, but it has a number of drawbacks:

- It falls short when it comes to static handling of names, e.g. we wouldn't be able to implement
  throwing a type error when the same variable is declared twice, because variable names are runtime
  info with (P)HOAS unlike with our approach where names exist at the Haskell's type level.
- When using (P)HOAS one has to actually bind a Haskell variable whenever they want to use a
  variable of the target language, which can be a hassle in cases when it's enough to merely know
  that the target language variable exists. For example, if you have a global target language
  variable that all procedures can read from / write to, then with HOAS you'd have to carry it
  around in all functions as an argument, while with our approach we can simply reassure the type
  checker that the variable exists using a 'declare'-like construct or introduce 'gget' and 'gset'
  for global variable reading and global variable writing etc. Point is, our approach is quite more
  flexible than (P)HOAS.
- Having to use Haskell names with (P)HOAS necessarily entails having to avoid clashing with names
  of common Haskell functions. It would be annoying to force the user to avoid shadowing names
  like @sum@, @null@, @and@, @elem@, @even@, @id@, @map@, @last@, @length@ or hide them from the
  implicitly imported Prelude. We even use the @length@ in our own tests!

Another thing we could do differently is use the @OverloadedLabels@ extension instead of
@OverloadedRecordDot@, but the simplest approach would require us to write @f (get #x)@ instead of
@f get.x@, which doesn't appear to be an improvement at all, so if we were to use overloaded labels,
we'd probably have to commit to @#x@ meaning different things in different contexts (for example
@set #x@ would elaborate one way and @#x + #y@ would elaborate a completely different way), which
sounds like it's not gonna play very nice with type inference unless we use @INCOHERENT@ pragmas,
which are brittle. Plus those hashes seem like a lot of visual noise, perhaps simply due to the
sheer amount of ink in them.
-}
