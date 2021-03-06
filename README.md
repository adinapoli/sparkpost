[![Build Status](https://travis-ci.org/adinapoli/sparkpost.svg?branch=master)](https://travis-ci.org/adinapoli/sparkpost)
[![Coverage Status](https://img.shields.io/coveralls/adinapoli/sparkpost.svg)](https://coveralls.io/r/adinapoli/sparkpost)

**Note: This is currently a spinoff of my own [mandrill](https://github.com/adinapoli/mandrill) library.
Expect alpha quality & incomplete software.**

# Haskell Client for the Sparkpost JSON API

This module implement a low-level, 1:1 mapping API to
the [sparkpost](http://sparkpost.com) transactional email service.

# Changelog

## Version 0.0.1.0

* Initial release (branching off from the `mandrill` library).


# Example

This package was built with pragmatism and reuse in mind. This means
this API comes in two flavours: an IO-based and an handy monad transformer
which can be plugged in your stack of choice.
Example:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Email.Validate
import Network.API.SparkPost

main :: IO ()
main = do
  case validate "foo@example.com" of
    Left err   -> print $ "Invalid email!" ++ show err
    Right addr -> runSparkPost "MYTOKENHERE" $ do
      let msg = "<p>My Html</p>"
      res <- sendEmail (newTextMessage addr [addr] "Hello" msg)
      case res of
        SparkPostSuccess k -> liftIO (print k)
        SparkPostFailure f -> liftIO (print f)
```

# Supported API versions

* 1.0 (partially)
  - [Users call](https://mandrillapp.com/api/docs/users.JSON.html) - 100%
    + info.json
    + ~~ping.json~~ (as **doesn't return valid json!**)
    + ping2.json
    + senders.json
  - [Messages call](https://mandrillapp.com/api/docs/messages.JSON.html)
    + send.json

# Testing online API

To test the online API, first build the package with tests enabled:

```
cabal install --enable-tests
```

Then export an environment variable with your SparkPost **Test** token:

```
export SPARKPOST_API_KEY="YOURKEYGOESHERE"
```

And finally execute the testsuite:

```
cabal test
```

# Contributions
This library scratches my own itches, but please fork away!
Pull requests are encouraged to implement the part of the API
you need.
