# Mini Java

This project aims at building a compiler for Java programs containing only Integers, Booleans, Strings and Objects to x86-64.

Please consider looking at [this project's repository](https://github.com/leo-leesco/mini-java.git) for the latest version of this project. (note : I know I am late, so please consider looking at the project repo as a late submission… thanks in advance !)

## TODO

### Type checking

- [x] check for uniqueness of classes
- [x] check for valid inheritance relationships
- [x] check for acyclical inheritance
- [x] build class tree
- [ ] check for uniqueness of attributes (and methods : I forgot that we allow for overriding methods through inheritance in `Mini Java`, so I started implementing verification forbidding redefinition) across inheritance relations
- [ ] build attributes and methods tables
- [ ] type check methods and constructors

### Code generation

- [ ] in-memory representation of classes
  - [ ] attributes offset
  - [ ] point to super class
  - [ ] methods addresses
- [ ] cast and `instanceof`
