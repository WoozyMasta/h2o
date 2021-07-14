# H2O: Help to Options

H2O extracts CLI options from help text, and then exports as a shell completion script, or JSON.


## Features

* Parses manpages, or help texts, to extract command-line information including flags, options, and subcommands.
* Generates shell completion scripts (fish/zsh/bash) from the parsed information.
* Exports CLI information as JSON.
* Works as the backend for [vscode-H2O](https://marketplace.visualstudio.com/items?itemName=tetradresearch.vscode-h2o), a VS Code Extension for shell script development.


## How to use

```shell
# Generate fish completion script from `man ls` or `ls --help`.
h2o --command ls --format fish > ls.fish

# Export CLI info as JSON
h2o --command ls --format json

# Parse manpage text file
man ls | col -bx > ls.txt
h2o --file ls.txt --format fish > ls.fish
```


## [Linux only] Sandboxing

H2O may call arbitrary programs in your system to get help information so running them in a sandboxed environment is the way to go. Since v0.1.8 H2O internally calls commands using [bubblewrap](https://github.com/containers/bubblewrap) with following:

```shell
bwrap --ro-bind / / --dev /dev --tmpfs --unshare-all <command-name> --help
```

Then `<command-name>` can neither connect to the network or write to your filesystem. So please consider installing bubblewrap to your system. H2O automatically uses it if avaialble.


## TODOs

- [ ] Improve parsing to support bioinformatics tools as many as possible
- [ ] Improve extraction of subcommand descriptions


## Related projects
* [parse-help](https://github.com/sindresorhus/parse-help)
