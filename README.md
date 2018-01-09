# Ether – The base element

*Ether* is small base library for OCaml designed from the ground up for
consistent, safe and user-friendly development experience.

- **API Reference**: <http://odis.io/docs/ether/Ether>
- **Status:** Unreleased


### Goals

- **Consistency and predictability** on all levels is enforced by
following strict naming and structural conventions.
- **Simplicity** at the point of usage. Interfaces are defined once but
used repeatedly. The exported API must be simple and understandable,
optimized for the context in which it will be used.
- **Composability** of the components. The modules, types and interfaces
are expected to be flexible and mixed in new ways.
- **Portability:** no external dependencies apart from the standard
library. Stable language features are preferred.


## Installation

This library can be installed with `opam`:

    opam install ether

If you don't use `opam` consult the [`ether.opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online](http://odis.io/docs/ether/Ether) or via `odig doc
ether`.


## License

Ether is distributed under the ISC license. See The `LICENSE` for more details.

