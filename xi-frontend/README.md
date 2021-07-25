# Xi-Frontend


This is the frontend Rust library for Aplite. 
Given a file of Aplite code (a module), the frontend is responsible for parsing it to a `Module` data structure, which is roughly a list of `module_items` (plus import dependencies). A `module_item` in Aplite, like many other languages, is a top-level definition.

## Workflow
The parsing is done as follows:

- The Aplite file is parsed through a tree-sitter library written for Aplite. The relevant code is in `./tree-sitter-aplite/grammar.js`. Tree-sitter generates a syntax tree from the Aplite file string.
- The string, together with the syntax tree is parsed into `./src/rowan_ast.rs`, which uses the syntax tree to turn the string into a Rowan_ast, which is what rust-analyzer uses for parsing Rust files. It is a tree-like data structure that is loseless, that is, every node that it visits still remembers the entire tree structure.
- Each top level statement (let & fn, as well as ffi, import and transport) are separated as module_items. We do this one by one is `./src/lib.rs`.
- `./src/resolve.rs` takes top level statement and replaces each declared variable with a unique identification number (uid), as well as adding it into the context. It also replace each non-declared variable with its uid by looking into the context.
- `./src/desugar.rs` desugars each statement into basic lambda expressions, which is what CoC is build upon. However, the resulting syntax, `Judg_ment`, has missing type information.
- `./src/type_inference.rs` uses an bi-directional type checking to fills in the missing type information, thus resulting in a `Judgment`. 
- The `Judgment`, together with all the metadata are bundled together in a `module_item`, a Aplite file is parsed to a `module` which is a list of `module_items`. 

Note that each module_item "imports" the other module_items above it. That is, if we have 
```
let f = ...
let g = f(5)
```
then in the module_item corresponding to `g`, `f` is considered as a free-variable, that is, the only information we have about `f` is its type, and NOT its implementation. 

The hiding of implementation, and that all usage of a function is encoded in its type, is a strong consistency condition that is enforced in Aplite. 
