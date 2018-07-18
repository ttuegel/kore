# Kore parser

To build: `stack build`.
All dependencies are managed by stack.

To run: `stack exec kore-parser FILE`.

To run the tests:
`stack test --coverage`
or
`stack test --no-keep-going`
or
`stack test --test-arguments --hide-successes`.
If you need stack traces, then you probably want something like
`stack test --executable-profiling --test-arguments --hide-successes`

To regenerate the golden data for regression tests:
`stack test --no-keep-going --ta --accept`

To generate documentation: `stack build --haddock`.

To test parsing performance:

1. Run the command at the top of `src/test/performance/parsing-base.almost-kore`
   to generate input files.
1. `stack build`
1. `time stack exec kore-parser ../../test/performance/parsing-512.kore -- --noverify --noprint`
