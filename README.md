# H2O: Parse CLI help texts / manpages and extract command options

## Demo

[Screencasts here...]

## What's this?

* Parse a manpage or a help text to extract command-line options
* Generate shell completion scripts (fish/zsh/bash) from the parsed information
* [In progress] VS Code extension for shell scripting


## How to use

```
h2o --help > ./h2o.txt
h2o ./h2o.txt --name h2o --shell fish > h2o.fish
```


## Related projects
* [parse-help](https://github.com/sindresorhus/parse-help)
