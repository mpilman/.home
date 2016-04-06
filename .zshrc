# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mpilman"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew common-aliases git-extras osx tmux zsh_reload)
DEFAULT_USER=mpilman

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
alias -s cc=vim
alias -s c=vim
alias -s cpp=vim
alias -s h=vim
alias -s hpp=vim

export PATH=/opt/local/bin:/usr/local/bin:/usr/local/sbin:/opt/local/sbin:~/.cabal/bin:$PATH
export EDITOR=vim

if [[ $HOST == bach* ]]
then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/local/mpilman/software/lib:/local/mpilman/software/lib64
    export PATH=/local/mpilman/software/bin:$PATH
    export C_INCLUDE_PATH=$C_INCLUDE_PATH:/local/mpilman/software/include
    export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/local/mpilman/software/include
    export LIBRARY_PATH=/local/mpilman/software/lib:/local/mpilman/software/lib64
    
    export http_proxy='http://proxy.ethz.ch:3128'
    export https_proxy='http://proxy.ethz.ch:3128'
fi
