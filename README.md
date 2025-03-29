## SchemeView

This is a simple hacky tool for generating and viewing block diagrams
that describe port connections among SystemVerilog modules.
Right now it works okay for CVA6. Not guaranteed to work elsewhere.

### Dependencies

You need to have Rust installed of course.
Diagrams are drawn using Graphviz. Make sure `dot` is in your `PATH`.

### Usage

First we need to index the SystemVerilog source files.

```
schemeview generate -I <include-path> -o <output-index-file> <sv-source-file> ...
```

Then we are ready to view the diagrams. Launch a web server:

```
schemeview serve <index-file> <top-module-name>
```

By default, it listens at `127.0.0.1:8000`, but you can change that with `--listen <addr>`.

#### Example

Take CVA6 as an example. We do

```
find cva6/core -name '*.sv' -type -f -exec schemeview generate -I cva6/core/includes -o cva6.json {} +
```

When this is complete, a file `cva6.json` is generated.

Now start the server.
We specify the `cva6` module as the top-level module.

```
schemeview serve cva6.json cva6
```

### License

[AGPLv3](LICENSE)

