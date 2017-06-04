# Snipcheck

[![Build Status](https://travis-ci.org/nmattia/snipcheck.svg?branch=master)](https://travis-ci.org/nmattia/snipcheck)

Snipcheck makes sure that the code snippets in markdown files are up-to-date.

This is very much a work in progress. The only function currently available is

``` haskell
checkMarkdownFile :: FilePath -> IO ()
```

that will run shell snippets and errored out if the output doesn't match the
snippet. You can skip some of the output with `...`.

## Example:

    # Some title

    some markdown content

    ``` shell
    $ echo foo; echo bar; echo baz; echo qux
    foo
    ...
    qux
    ```

    some more content


## Release check-list

1. Make sure you're on master

1. Bump the version in `snipcheck.cabal`:

> Given a version number MAJOR.MINOR.PATCH, increment the:
>
> MAJOR version when you make incompatible API changes,
> MINOR version when you add functionality in a backwards-compatible manner, and
> PATCH version when you make backwards-compatible bug fixes.

1. Run `stack update --pvp-bounds both .` to upload `snipcheck` to `hackage`
1. Commit the updated `snipcheck.cabal` file.
