* Phantheck - Approximates dependent types with


Requires closed type families which are present in at least GHC 7.7.20140103

* TODO:
- Parse (is GHC API the only option? haskell-src-exts seemed to fail) for QuickCheck functions instead of regex
- Is there a good way to give the template functions a filename to operate on while avoiding the TH stage restriction?
- Is there a less noisy way of splicing in postconditions?
- Is there a clearer way of doing the templating (i.e. `checkPost`)?
