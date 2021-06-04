# H2O: Help to Options

H2O extracts CLI options from help text, and then exports as a shell completion script, or JSON.


## Demo

[Screencasts here...]

## Features

* Parses a help text, or manpage, to extract command-line options and subcommands.
* Generates shell completion scripts (fish/zsh/bash) from the parsed information.
* Exports CLI options / subcommand information in JSON.
* Works as the backend for [vscode-H2O](https://marketplace.visualstudio.com/items?itemName=tetradresearch.vscode-h2o), a VS Code Extension for shellscript development.


## How to use

```shell
# Generate fish completion script from `ls --help` command.
h2o --command ls --shell fish > ls.fish

# Export info from `ls --help` in JSON
h2o --command ls --json

# Parse manpage text file
man ls | col -b > ls.txt
h2o --file ls.txt --shell fish > ls.fish
```


## TODOs

- [ ] Support subcommands in bash/zsh completions
- [ ] Improve parsing to support bioinformatics tools as many as possible.



## Related projects
* [parse-help](https://github.com/sindresorhus/parse-help)
