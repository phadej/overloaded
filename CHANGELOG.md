# 0.3.1

- Add `Overloaded:Constructors`

# 0.3

- Add `Overloaded:RebindableApplications`
- Add `Overloaded:CodeLabels` and `Overloaded:CodeStrings`
  (they don't work well though due how Typed Template Haskell is type-checked).
- Change class hierarcy in `Overloaded.Category`

# 0.2.1

- Add `Overloaded:Categories`, which makes `Arrows` notation desugar to
  categories, a bit like in Conal Elliot's *Compiling to Categories*.
- Add `Overloaded:Do`, which is like *Local Do*
- Add `Overloaded:Unit`, which overloads value `()` to be whatever you want
- GHC-8.10 support

# 0.2

- Make infixr 5 cons
- Make Vec, NP and POP instances match more eagerly
- Add `Overloaded.Lists.Bidi`
- Add `Seq`, `Map` and `IntMap` `Cons` and `Nil` instances
- Add `bin` and `ral` instances

# 0.1.3

- Add `Nil` and `Cons` `Set` and `IntSet` instanes
- Add 'fromList' function for homogeneous things.
- Enable `PolyKinds` for `Lists` instances.

# 0.1.2

- Add `Overloaded:RecordFields` type-checker plugin to emulate `HasField` from [`GHC.Record.Compat`](https://hackage.haskell.org/package/record-hasfield-1.0/docs/GHC-Records-Compat.html)
- Add `Overloaded:IdiomBrackets` to turn `TemplateHaskellQuotes` into [*idiom brackets*](http://www.staff.city.ac.uk/~ross/papers/Applicative.html)

# 0.1.1

- Add `Overloaded.TypeNats` and `Overloaded.TypeSymbols`
- Add `FromSymbol s Day` instance validating `yyyy-mm-dd` format

# 0.1

- Initial release
