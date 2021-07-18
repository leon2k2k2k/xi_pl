# Xi programming language

This is a prototype for Aplite, which will eventually become a good programming language.

This is currently a cargo workspace, with the following crates inside:

- xi-core - Defines the type theory and the fundamental data structures of Aplite.
- xi-backends - This contains the backends to Aplite, functions which take a Judgment and give you a string.
- xi-cli - The cli and main entry point. It also contains basic Aplite code in xi-cli/tests.
- xi-runtimes - Takes strings from xi-backends and runs them.
- xi-kernels - Functions from Judgment to Judgment
- xi-uuid - A utility crate to generate unique numbers.
- xi-k8s - a kubernetes library/backend for Aplite.
