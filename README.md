cli
===

[![Build Status](https://travis-ci.org/vincenthz/hs-cli.png?branch=master)](https://travis-ci.org/vincenthz/hs-cli)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Documentation: [cli on hackage](http://hackage.haskell.org/package/cli)

Option parser in DSL form, and display utilities for command line interfaces.

Option Parsing
--------------

Basic program looks like:

```haskell
defaultMain $ do
    f1 <- flag ..
    f2 <- argument ..
    action $ \toParam ->
        something (toParam f1) (toParam f2) ..
```

with subcommands:

```haskell
defaultMain $ do
    subcommand "foo" $ do
       <..flags & parameters definitions...>
       action $ \toParam -> <..IO-action..>
    subcommand "bar" $ do
       <..flags & parameters definitions...>
       action $ \toParam -> <..IO-action..>
```

A Real Example:

```haskell
main = defaultMain $ do
    programName "test-cli"
    programDescription "test CLI program"
    flagA    <- flag $ FlagShort 'a' <> FlagLong "aaa"
    allArgs  <- remainingArguments "FILE"
    action $ \toParam -> do
        putStrLn $ "using flag A : " ++ show (toParam flagA)
        putStrLn $ "args: " ++ show (toParam allArgs)
```

License
-------

The source code of cli is available under the [BSD 3-Clause license](https://opensource.org/licenses/BSD-3-Clause), see `LICENSE` for more information.
