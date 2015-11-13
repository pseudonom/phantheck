Phantheck
=========

Integrate QuickChecked properties into the type system with phantom types

What
----

Phantheck allows you to annotate functions with preconditions and postconditions. Conditions are specified with phantom type-level lists. Applying a function to a parameter which does not satisfy the preconditions will cause a compile-time error. Postconditions are not specified by assertion but by splicing in QuickChecked properties. `Demo.hs` may make things clearer.

How to use
----------

Currently, things are very much hard-coded and proof-of-concept. To play with the proof of concept, run a command like:

```sh
STRICT=False stack build && stack clean && STRICT=True stack build
```.

As you can see `phantheck` does two phases of compilation. In the first phase, it only checks for the existence of property tests and assumes they pass. In the second phase, it runs the tests compiled in the first phase and verifies that they actually succeed.

TODO
----

- Lots of clean-up and less hard-coding

- Do actual parsing instead of regexing and make things more robust

- Make `Property` properly extensible

- More complex use-cases and examples
