# parsable-hs
Parsable and Printable Haskell classes

---

The Data.Parsable module contains two simple classes, `Parsable` and `Printable`.
The motivation behind this package is to support parsing and printing for arbitrary
text files.

There is an implicit "soft isomorphism" between `parser` and `toString`.
(Successfully parsing a string and then running 'toString' on the result
should result in the original string.)

`Parsable` instances must return a `PartialParse` wrapping the data. This can
be done easily with the `endPartial` function.

For instance:

```
parser = endPartial $ count 5 anyChar
```

## Language extensions

Because `parser` and `parser'` do not take any arguments, it may be necessary
to explicitly declare the type of `t` for these functions.

It may be helpful to enable and use the `TypeApplications` and possibly
`ScopedTypeVariables` extensions. This extends to helper functions that use
`parser` and `parser'`, such as `runParsable` and `runParsable'`.

Look at the `Language Extensions` section of the GHC documentation for
instructions on how to use these extensions.
