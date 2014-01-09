Phantheck
=========

Approximate dependent types with phantom types and QuickChecked properties

What
------

Phantheck allows you to annotate functions with preconditions and postconditions. Conditions are specified with phantom type-level lists. Applying a function to a parameter which does not satisfy the preconditions will cause a compile-time error. Postconditions are not specified by assertion but by splicing in QuickChecked properties. `Demo.hs` may make things clearer.

Dependencies
------------

Uses closed type families which are present in at least GHC 7.7.20140103

Questions
---------

- Parse (is GHC API the only option? `haskell-src-exts` seemed to fail) for property verifiers instead of regex
- Is there a good way to give the template functions a filename to operate on while avoiding the TH stage restriction?
- Is there a less noisy way of splicing in postconditions?
- Is there a clearer way of doing the templating (i.e. `checkPost`)?
