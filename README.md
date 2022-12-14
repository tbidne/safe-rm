<div align="center">

# safe-rm

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/safe-rm?include_prereleases&sort=semver)](https://github.com/tbidne/safe-rm/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/safe-rm?color=blue)](https://opensource.org/licenses/MIT)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/nix.yaml?branch=main&label=nix&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/cabal.yaml)
[![stack](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/stack.yaml?branch=main&label=stack&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/stack.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/safe-rm/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/style.yaml)

</div>

---

### Table of Contents

- [Introduction](#introduction)
  - [Usage](#usage)
- [Configuration](#configuration)
- [Commands](#commands)
  - [Delete Commands](#delete-commands)
    - [Delete](#delete)
    - [Permanent Delete](#permanent-delete)
    - [Empty](#empty)
  - [Restore Commands](#restore-commands)
    - [Restore](#restore)
  - [Information Commands](#information-commands)
    - [List](#list)
    - [Metadata](#metadata)
- [Building](#building)
  - [Cabal](#cabal)
  - [Stack](#stack)
  - [Nix](#nix)

# Introduction

`safe-rm` is a CLI tool for deleting files (like `rm`), but instead of permanently deleting files,
 moves them to a trash location (like Windows' recycle bin, or OSX's trash).

## Usage

```
Safe-rm: A tool for deleting files to a trash directory.

Usage: sr [-c|--config [none|PATH]] [-t|--trash-home PATH]
          [--log-level [none|error|warn|info|debug]] COMMAND [--version]

Safe-rm moves files to a trash directory, so they can later be restored or permanently deleted. It is intended as a safer alternative to rm. See github.com/tbidne/safe-rm#readme for full documentation.

Available options:
  -c,--config [none|PATH]  Path to the toml config file. Can be the string
                           'none' -- in which case no toml config is used -- or
                           a path to the config file. If not specified then we
                           look in the xdg config directory e.g.
                           ~/.config/safe-rm/config.toml
  -t,--trash-home PATH     Path to the trash directory. This overrides the toml
                           config, if it exists. If neither is given then we use
                           the xdg home directory e.g. ~/.trash
  --log-level [none|error|warn|info|debug]
                           The file level in which to log. Defaults to none.
  -h,--help                Show this help text

Delete Commands
  d                        Moves the path(s) to the trash.
  x                        Permanently deletes path(s) from the trash.
  e                        Empties the trash and deletes the index.

Restore Commands
  r                        Restores the trash path(s) to their original
                           location.

Information Commands
  l                        Lists all trash contents and metadata.
  m                        Prints trash metadata.

Version: 0.1
```

# Configuration

`safe-rm` can be configured by either CLI args or a `toml` config file. A path to the config file can be given with the `-c` option. Otherwise we search in the xdg config e.g. `~/.config/safe-rm/config.toml`. In general, if an option can be specified in both the config file and on the CLI (e.g. `--trash-home`), then the CLI takes priority.

See [default.toml](./examples/config.toml) for a description of the `toml` file.

# Commands

This section describes the possible commands, along with their specific options.

## Delete Commands

### Delete

**Usage:**

```
Usage: sr d PATHS...
  Moves the path(s) to the trash.

Available options:
  -h,--help                Show this help text
```

**Examples**

```
# moves paths "foo", "bar", and "baz" to the trash
$ sr d foo bar baz
```

### Permanent Delete

**Usage:**

```
Usage: sr x [-f|--force] PATHS...
  Permanently deletes path(s) from the trash.

Available options:
  -f,--force               If enabled, will not ask before deleting each path.
  -h,--help                Show this help text
```

**Examples**

```
# permanently deletes "foo", "bar", and "baz" from the trash directory
$ sr x -f foo bar baz
```

### Empty

**Usage:**

```
Usage: sr e
  Empties the trash and deletes the index.

Available options:
  -h,--help                Show this help text
```

**Examples**

```
$ sr e
```

## Restore Commands

### Restore

**Usage:**

```
Usage: sr r PATHS...
  Restores the trash path(s) to their original location.

Available options:
  -h,--help                Show this help text
```

**Examples**

```
# deleting "foo" and "baz" first
$ sr d foo baz

# restore "foo" and "baz" to their original locations
$ sr r foo baz
```

## Information Commands

### List

**Usage:**

```
Usage: sr l [--format ([m]ulti | [s]ingle)] [-n|--name-trunc NAT]
            [-o|--orig-trunc NAT] [-s|--sort (name|size)] [-r|--reverse-sort]
  Lists all trash contents and metadata.

Available options:
  --format ([m]ulti | [s]ingle)
                           Determines the output format. Defaults to 'single'
                           i.e. each trash entry is printed in a single line, in
                           tabular form.
  -n,--name-trunc NAT      Truncates the name to NAT chars. Multiline option is
                           unaffected.
  -o,--orig-trunc NAT      Truncates the original path to NAT chars. Multiline
                           option is unaffected.
  -s,--sort (name|size)    How to sort the list. Defaults to name.
  -r,--reverse-sort        Sorts in the reverse order.
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ sr d foo bar baz

# list contents
$ sr l

Name       | Type       | Size    | Original               | Created
--------------------------------------------------------------------------------
bar        | File       | 41.35M  | /home/tommy/Dev/tom... | 2022-10-28 15:33:18
baz        | File       | 45.61M  | /home/tommy/Dev/tom... | 2022-10-28 15:33:18
foo        | File       | 24.38M  | /home/tommy/Dev/tom... | 2022-10-28 15:33:18

Entries:      3
Total Files:  3
Log size:     1.65K
Size:         111.35M
```

### Metadata

**Usage:**

```
Usage: sr m
  Prints trash metadata.

Available options:
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ sr d foo bar baz

# list contents
$ sr m

Entries:      3
Total Files:  3
Log size:     4.89K
Size:         111.35M
```

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of
  * [ghc 9.2](https://www.haskell.org/ghcup/)
  * [ghc 9.4](https://www.haskell.org/ghcup/)
* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `cabal` + `ghcup` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `safe-rm` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Stack

Like `cabal`, `safe-rm` can be built locally or installed globally (e.g. `~/.local/bin/`) with `stack build` and `stack install`, respectively.

## Nix

### From source

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `safe-rm` can be built with `nix build`, which will compile and run the tests.

To launch a shell with various tools (e.g. `cabal`, `hls`), run `nix develop`. After that we can launch a repl with `cabal repl` or run the various tools on our code. At this point you could also build via `cabal`, though you may have to first run `cabal update`. This will fetch the needed dependencies from `nixpkgs`.

### Via nix

Because `safe-rm` is a flake, it be built as part of a nix expression. For instance, if you want to add `safe-rm` to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    safe-rm-src.url= "github:tbidne/safe-rm/main";
  };

  outputs = { self, nixpkgs, safe-rm-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
      safe-rm = safe-rm-src.packages."${system}".default;
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs safe-rm; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, safe-rm, ... }:

{
  environment.systemPackages = [
    safe-rm
  ];
}
```