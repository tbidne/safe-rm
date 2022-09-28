<div align="center">

# safe-rm

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/safe-rm?include_prereleases&sort=semver)](https://github.com/tbidne/safe-rm/releases/)
[![MIT](https://img.shields.io/github/license/tbidne/safe-rm?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/safe-rm/nix/main?label=nix%209.2&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/nix.yaml)
[![cabal](https://img.shields.io/github/workflow/status/tbidne/safe-rm/cabal/main?label=cabal&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/cabal.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/safe-rm/stack/main?label=stack&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/stack.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/safe-rm/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/safe-rm/actions/workflows/style.yaml)

</div>

---

### Table of Contents

- [Introduction](#introduction)
  - [Usage](#usage)
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

# Introduction

`safe-rm` is a CLI tool for deleting files (like `rm`), but instead of permanently deleting files,
 moves them to a trash location (like Windows' recycle bin, or OSX's trash).

## Usage

```
Safe-rm: A tool for deleting files to a trash directory.

Usage: sr COMMAND [-v|--version]

Safe-rm moves files to a trash directory, so they can later be restored or permanently deleted. It is intended as a safer alternative to e.g. rm.

Available options:
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

# Commands

## Delete Commands

### Delete

**Usage:**

```
Usage: sr d [-t|--trash PATH] PATHS...
  Moves the path(s) to the trash.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
  -h,--help                Show this help text
```

**Examples**

```
# moves paths "foo bar baz" to the default trash
$ sr d foo bar baz
```

### Permanent Delete

**Usage:**

```
Usage: sr x [-t|--trash PATH] [-f|--force] PATHS...
  Permanently deletes path(s) from the trash.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
  -f,--force               If enabled, will not ask before deleting each path.
  -h,--help                Show this help text
```

**Examples**

```
# permanently deletes "foo bar baz" from the trash directory
$ sr x -f foo bar baz
```

### Empty

**Usage:**

```
Usage: sr e [-t|--trash PATH]
  Empties the trash and deletes the index.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
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
Usage: sr r [-t|--trash PATH] PATHS...
  Restores the trash path(s) to their original location.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
  -h,--help                Show this help text
```

**Examples**

```
# deleting 'foo' first
$ sr d foo

# restore foo to original location
$ sr r foo
```

## Information Commands

### List

**Usage:**

```
Usage: sr l [-t|--trash PATH]
  Lists all trash contents and metadata.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ sr d foo bar baz

# list contents
$ sr l

Type:      File
Name:      bar
Original:  <path>/bar
Created:   2022-09-28 23:57:10 

Type:      Directory
Name:      baz
Original:  <path>/baz
Created:   2022-09-28 23:57:10 

Type:      File
Name:      foo
Original:  <path>/foo
Created:   2022-09-28 23:57:10 

Entries:      3
Total Files:  2
Size:         246.00B 
```

### Metadata

**Usage:**

```
Usage: sr m [-t|--trash PATH]
  Prints trash metadata.

Available options:
  -t,--trash PATH          Path to the trash directory. If none is given we
                           default to XDG/.trash e.g. ~/.trash.
  -h,--help                Show this help text
```

**Examples**

```
# deleting files/directories first
$ sr d foo bar baz

# list contents
$ sr m

Entries:      3
Total Files:  2
Size:         246.00B 
```