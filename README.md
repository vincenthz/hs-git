git
===

[![Build Status](https://travis-ci.org/vincenthz/hs-git.png?branch=master)](https://travis-ci.org/vincenthz/hs-git)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

git is a reimplementation of git storage and protocol in pure haskell.

what it does do:

* read loose objects, and packed objects.
* write new loose objects
* git like operations available: commit, cat-file, verify-pack, rev-list, ls-tree.

what is doesn't do:

* reimplement the whole of git.
* checkout's index reading/writing, fetching, merging, diffing.
