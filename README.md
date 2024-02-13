it's a haskell rogue-like!

### build

```console
$ nix build
```
In case of successfull build a `./result` symlink will be created with binaries.

### run

Without `git clone` build and run the latest version in a single command:
```console
$ nix run git@github.com:adlaika/roskell.git
```

When inside the working directory of the cloned git repository.
```console
$ nix run
```

or if you have already run `nix build` it should have created a `./result` symlink:

```console
$ result/bin/roskell
```

Exit with `q` or Ctrl+C. After exitting you might want to fix your terminal with `reset` command.

### develop

```console
$ nix develop
$ cabal repl
```
