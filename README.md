# lgrep

## 1. Introduction
The standard unix tool `grep` is used to search for lines matching a given pattern. This tool
is intended to do something similar.

The author had a need for searching through a large set of UTF-16 encoded files and found
the standard unix grep tool from MSYS lacking, mainly because it does not support UTF-16.
This tool is noticably faster, although that is not a primary concern.

## 2. Usage

## 2.1 `maplines` `mapfile`
This function walks over a given file line by line, invoking a callback on each line.
Lines are delimited by a given end of line sequence, which may be either a string or a sequence of
unsigned bytes.

A restart is bound so that if a line cannot be decoded to a string, the caller may optionally
ignore this file and continue searching. 

## 2.2 `mapgrep`
This functions walks over a given file (using mapfile) searching for only those lines which
match a given predicate. A callback is invoked on each matching line. Files are searched
recursively into subdirectories, if the `RECURSIVEP` option is set.

If any lines cannot be decoded to a string, a warning is signalled and the file is ignored.

## 2.3 `grep-if`
This function searches through a set of files, invoking a callback on each line of each file.

## 2.4 `grep`, `grep*`
These functions are conveience wrappers. `grep` returns a list of all matches, `grep*` prints
out matching lines to `*STANDARD-OUTPUT*` in a similar way to unix grep.

## 2.5 `diff`
This function compares two files and prints differences to `*STANDARD-OUTPUT*` in the unified diff
format. 

## 3. License
Licensed under the terms of the MIT license.

## 4. Notes
The diffing algorithm was translated from the original Python program at (blog.robertelder.org/diff-algorithm). 

Frank James
2019



