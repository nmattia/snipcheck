# Snipcheck

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

