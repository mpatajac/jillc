# Jill programming language

Jill is a functional programming language built for the [Nand2Tetris](https://www.nand2tetris.org/) platform, as an alternative to the original *Jack* high-level language.

It is designed as a drop-in replacement for Jack, as it uses the same VM instruction set and underlying *HACK* architecture, and follows similar design principles (willing to sacrifice ease of use to favour ease of implementation), while offering an alternative to Jack's very object-oriented, verbose style (*I like to think of Jill as Jack's more elegant, modern sister*).

### Some notable features:
- functions as first-class citizens (abilty to store them in variables, pass them on to other functions as arguments, and return from functions as a result)
- optimized tail-call recursion to use constant stack space (single stack frame)
- data modeling using algebraic data types with primitive pattern-matching (per type variant)
  - note that, as with Jack, all variables are still effectively 16-bit integers, therefore Jill is dynamically typed
- minimal lanugage design
  - only 3 main concepts (types, variables and functions)
  - expressions can only be literals, variables or function calls
- expanded standard library which is lazily-generated (instructions are generated only for modules and functions which were used in codebase)
- common design choices of functional languages (no loops, variables are immutable, code is organized into modules etc.)


## Usage

TODO: add link to language tour

### Project structure

At the root of your project you should have two directories - `/src`, which will contain all of your Jill code, and `/bin`, which will contain the generated `.vm` files.

All of your project's Jill code should be written in `.jill` files, located in the `/src` directory at the root of your project, or one of its subdirectories.

Once compiled, your projects' VM instructions will be located in the `bin` directory, at the root of your project.

An example project structure:
```
project_root
|  README.md
|
|__src
|  |  Main.jill
|  |  Foo.jill
|  |  ...
|  |
|  |__Model
|  |  |  Bar.jill
|  |  |  Baz.jill
|  |  |  ...
|
|__bin
|  |  Main.vm
|  |  Foo.vm
|  |  Model_Bar.vm
|  |  Model_Baz.vm
|  |  ...
```

### Examples
You can find several examples of Jill code/projects in the `examples` directory.

## Setup

### Pre-built
You can find a pre-built Windows executable in the latest [release](https://github.com/mpatajac/jillc/releases).

### Manual

#### Prerequisites
Make sure you have [the Rust toolchain](https://www.rust-lang.org/tools/install) installed and updated to a recent version.

#### Installation
After cloning this repository, you can either build and run the compiler in a single step:
```shell
$ cargo run -- path_to_jill_project_root
```

or build the compiler using
```shell
$ cargo build
```
and use the binary in `./target/debug` to compile your Jill codebase
```shell
$ jillc [path_to_jill_project_root]
```

For more information on the compiler itself, you can always check the provided manual with `jillc --help`.
