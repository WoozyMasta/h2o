# H2O: Parse CLI help texts / manpages and extract command options

## Demo

[Screencasts here...]

## What's this?

* Parse a manpage or a help text to extract command-line options
* Generate shell completion scripts (fish/zsh/bash) from the parsed information
* [In progress] VS Code extension for shell scripting


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

## Related projects
* [parse-help](https://github.com/sindresorhus/parse-help)
