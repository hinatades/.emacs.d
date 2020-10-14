# dotfiles

My OS X Dotfiles.

## Installation

First of all, install the following with [Homebrew](https://brew.sh/).

```sh
$ brew install zsh zsh-completion vim tmux reattach-to-user-namespace the_silver_searcher fzf ripgrep clang-format peco ghq hub
```

Then clone the repogitory with [ghq](https://github.com/x-motemen/ghq)

```sh
$ ghq get https://github.com/hinatades/dotfiles.git
```

## Setup

```sh
$ ./set-symboliclink.sh
```

### Install zsh themes

```sh
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```
