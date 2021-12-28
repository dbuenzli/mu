mu — Declarative music for OCaml
=================================
%%VERSION%%

Mu is an OCaml module for declaring music. It provides a simple type
for representing music and combinators to define and compose it.

Music values are interpreted by renderers. Mu has built-in support for
rendering music values to MIDI files.

Mu is distributed under the ISC license. It has no dependencies.

Homepage: <https://erratique.ch/software/mu>

# Installation

Mu can be installed with `opam`:

    opam install mu

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc mu`.

A few basic sample programs can be found in the [test](test/)
directory.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/mu/doc
[ocaml-forum]: https://discuss.ocaml.org/

# Acknowledgments

Mu is essentially a port of the [Euterpea] library described in:

* Paul Hudak and Donya Quick. [The Haskell School of Music][hsom]. 2018

[hsom]: https://doi.org/10.1017/9781108241861
[euterpea]: https://www.euterpea.com/


