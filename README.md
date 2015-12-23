# kalerl

##Description

An Erlang implementation of the [Kaleidoscope language from the LLVM tutorial](http://llvm.org/docs/tutorial/index.html). It's not a straight port as it targets BEAM, the Erlang VM, instead of native code. The project is a work in progress for the purpose of learning how to generate code targeting BEAM.

##Dependencies

* rebar
* make
* erlando (automatically downloaded and built by rebar)

##Build

To build kalerl compiler just run make.

    $ make

This will place the `kalerl` executable in the `bin` subdirectory.

##Use

The `kalerl` compiler can only take one parameter, the file to compile. For example:

    $ ./bin/kalerl test/add.kalerl

This will generate a BEAM file in the same directory as the source file. In this case it will be `test/add.beam`. From here, the BEAM file can be used just like any other; all functions are exported by default.

    $ cd test
    $ erl
    Eshell V7.2  (abort with ^G)
    $ 1> l(add).
    {module,add}
    
The above moves to the `test` directory, starts the Erlang shell, and loads the `add` BEAM file just built. To call a function:

    $ 2> add:add(3.0).
    5.0
    
Also, all kalerl modules export a `main/0` function that executes all the top level expressions.

    $ 3> add:main().
    5.0

##Language Differences

There are a few difference between kalerl and Kaleidoscope as defined by the LLVM tutorial. The most obvious is there is no REPL in kalerl; it assumes reading and writing from files.

Another difference is the handling of externs. kalerl doesn't handle calling out to C functions; instead it supports calling into Erlang modules. To accomplish that the language syntax changed to support designating the external module name. For example:

    extern foo(a) in bar
    
declares that there is a function `foo/1` in module `bar`. After this declaration, kalerl code can call `foo/1` like any other kalerl function and it will do the right thing.
