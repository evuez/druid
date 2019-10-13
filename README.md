# druid

Trying to parse Elixir with Megaparsec.

You can try it with `readExpr "a + b"` or `readSource "path/to/file.ex"` in ghci (`stack ghci`).

You can also install it with `stack install` and then run `druid path/to/file.ex`.

## Tests

Run the tests with `stack test`.

## TODO

 - [ ] `?` syntax for codepoints
 - [ ] String interpolation
 - [ ] Allow `_` in numbers
