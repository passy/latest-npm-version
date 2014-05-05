# latest-npm-version

Trying to understand Lenses and Pipes by "porting" @sindresorhus'
[latest-version](https://github.com/sindresorhus/latest-version/).


## Install

```
> cabal install npm-latest-version
> npm-latest-version yo
yo 1.2.0
```

Now get yourself a nice hot cup of tea, take a bath, file your tax return and if
you're lucky cabal will be finished by then.

## Dev

```
> cabal sandbox init
> cabal install --only-dependencies
> cabal run -- yo
```
