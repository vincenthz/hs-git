git
===

git is a reimplementation of git storage and protocol in pure haskell.

what it does do:

* read loose objects, and packed objects.
* write new loose objects
* git like operations available: commit, cat-file, verify-pack, rev-list, ls-tree.

what is doesn't do:

* reimplement the whole of git.
* checkout's index reading/writing, fetching, merging, diffing.
