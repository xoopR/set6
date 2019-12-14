# set6 0.1.1

### Patches
- `absComplement` method is now deprecated, instead use `setcomplement` and omit the `y` argument
- Fixed error in `contains` default caused by `%inset%`
- Improved printing of `SpecialSet`s when `zero == TRUE`
- Added `UniversalSet` for the set containing all elements
- Changed default `universe` of sets to `UniversalSet`
- Coercions now error instead of producing a message when they fail
- On construction, `Set`s no longer guess the set class, instead an extra `class` argument is added to give a set the `typed` property
- The internal `Set` structure is slightly changed so that set elements are now stored in lists by default, which is only changed if the set is `typed`
- Added `element` argument to `Set` constructor, which takes a `list`. This is more efficient if passing lists of lists or lists of multiple types, and in line with the `FuzzySet` constructor
- Improved printing of `ConditionalSet`s

# set6 0.1.0

- `set6` upgrades the `sets` package to R6. Many forms of mathematical sets are implemented, including (countably finite) sets, tuples, intervals (countably infinite or uncountable), and fuzzy variants. Wrappers extend functionality by allowing symbolic representations of complex operations on sets, including unions, (cartesian) products, exponentiation, and differences (asymmetric and symmetric).
- See [the website](https://raphaels1.github.io/set6/) for more details and the project readme
- See [getting started vignette](https://raphaels1.github.io/set6/articles/set6.html) for a short tutorial and introduction
- `set6` is currently 'maturing', so whilst no major updates are planned they may happen. Constant minor updates should be expected.