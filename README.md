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

# ... and more
h2o --help
```


## Just want some shell completion scripts?
Try our [curated data repository](https://github.com/yamaton/h2o-curated-data) containing generated bash|zsh|fish completion scripts. They were created with H2O plus manual edits.


## TODOs

- [ ] Improve parsing of subcommands with descriptions
- [ ] Support more bioinformatics tools


## Related projects
* [parse-help](https://github.com/sindresorhus/parse-help)
