# druid

Trying to parse Elixir with Megaparsec.

You can try it with `readExpr "a + b"` or `readSource "path/to/file.ex"` in ghci (`stack ghci`).

You can also install it with `stack install` and then run `druid path/to/file.ex`.

[The tests](https://github.com/evuez/druid/blob/master/test/Spec.hs) should give you a good idea of what it can currently parse.

## Formatting

I'm using [hindent](https://github.com/chrisdone/hindent) to format the source files. Running `make format` will run it on all the source files.

## Tests

Run the tests with `make test` or `make test.watch` to automatically run the tests when a file changes.
