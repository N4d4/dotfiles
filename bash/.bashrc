# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Vim default editor (fix for ranger)
export VISUAL=vim
export EDITOR="$VISUAL"

# Used by ranger. Note that ranger doesn't handle absolute pathes.
#export TERMCMD="st"
# Used by mimeopen when launching applications with Terminal=true:
#export TERMINAL="st"

# Vi command line like
#set -o vi

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# Add .local/bin to PATH
if [ -d ~/.local/bin ];then
    PATH="$PATH:$HOME/.local/bin/"
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
#case $- in
#    *i*) ;;
#      *) return;;
#esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
#PROMPT_DIRTRIM=2

## SMARTER COMPLETION {{{ 
# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# Immediately add a trailing slash when autocompleting symlinks to directories
bind "set mark-symlinked-directories on"

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
bind Space:magic-space

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

### }}}
## DIRECTORY NAVIGATION {{{

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH="."

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

# Examples:
export dots="$HOME/dots/"
# export projects="$HOME/projects"
# export documents="$HOME/Documents"
# export dropbox="$HOME/Dropbox"
## }}}
## HISTORY SETTINGS {{{
# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Enable incremental history search with up/down arrows (also Readline goodness)
# Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

## }}}
## GENERAL ASPECT {{{

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Import colorscheme from 'pywal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
#deprecated (wal -tr &)
(cat ~/.cache/wal/sequences &)
# To add support for TTYs this line can be optionally added.
source ~/.cache/wal/colors-tty.sh

# Show filetype-icon with even-better-ls
LS_COLORS=$(ls_colors_generator)
run_ls() {
	ls-i --color=auto -w $(tput cols) "$@"
}
run_dir() {
	dir-i --color=auto -w $(tput cols) "$@"
}
run_vdir() {
	vdir-i --color=auto -w $(tput cols) "$@"
}
alias ls="run_ls"
alias dir="run_dir"
alias vdir="run_vdir"
# alias for normal ls,dir,vdir
alias nls="ls-i"
alias ndir="dir-i"
alias nvdir="vdir-i"

# Color man pages
man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;31m") \
		LESS_TERMCAP_md=$(printf "\e[1;31m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[1;32m") \
			man "$@"
}

## }}}
## PROMPT SETTINGS {{{

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

_PROMPT() {
    local EXITCODE="$?"
    local NAME="\[$(tput setaf 4)\]\u"
    local SEPARATOR="\[$(tput setaf 1)\]╺─╸"
    local FOLDER_PATH="\[$(tput setaf 8)\][\[$(tput setaf 3)\]\w\[$(tput setaf 8)\]]"
    local NEW_LINE="\n$\[$(tput setaf 7)\] "
#   PS1="\u╺─╸[\w]\n$ " 
    #PS1="\[$(tput setaf 1)\]a\[$(tput setaf 2)\]a\[$(tput setaf 3)\]a\[$(tput setaf 4)\]a\[$(tput setaf 5)\]a\[$(tput setaf 6)\]a\[$(tput setaf 7)\]a\[$(tput setaf 8)\]a\u╺─╸[\w]\n$ "
    PS1="$NAME$SEPARATOR$FOLDER_PATH$NEW_LINE" 
    unset _EXIT_STATUS_STR

}
PROMPT_COMMAND=_PROMPT

## }}}
## PROMPT TEST {{{
#https://www.reddit.com/r/linux/comments/2uf5uu/this_is_my_bash_prompt_which_is_your_favorite/
    #PS1="nada╺─╸[\w]" 
    #PS1="┌─╼ [\w]\n
    #     └────╼\";
#if [ "$color_prompt" = yes ]; then
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#else
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi
#unset color_prompt force_color_prompt

#__prompt_command() {
#    local EXITCODE="$?"
#
#    local HOSTCOLOR="5"
#    local USERCOLOR="3"
#    local PATHCOLOR="4"
#
#    PS1="\e[3${HOSTCOLOR}m \h  \e[3${USERCOLOR}m \u  \e[3${PATHCOLOR}m \w  \n";
#
#    if [ $EXITCODE == 0 ]; then
#        PS1+="\e[32m\$ \e[0m";
#    else
#        PS1+="\e[31m\$ \e[0m";
#    fi
#}

#_PROMPT() {
#    _EXIT_STATUS=$?
#    [ $_EXIT_STATUS != 0 ] && _EXIT_STATUS_STR="\[\033[1;30m\][\[\033[1;31m\]$_EXIT_STATUS\[\033[1;30m\]] "
#    PS1="nada\033[1;30m\]╺─╸$_EXIT_STATUS_STR\[\033[1;30m\][\[\033[0m\]\W\[\033[1;30m\]]\[\033[1;34m\];\[\033[0m\] "
#    unset _EXIT_STATUS_STR
#}
#PROMPT_COMMAND=_PROMPT

# Prompt
#if [ -n "$SSH_CONNECTION" ]; then
#export PS1="\[$(tput setaf 1)\]┌─╼ \[$(tput setaf 7)\][\w]\n\[$(tput setaf 1)\]\$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 1)\]└────╼ \[$(tput setaf 7)\][ssh]\"; else echo \"\[$(tput setaf 1)\]└╼ \[$(tput setaf 7)\][ssh]\"; fi) \[$(tput setaf 7)\]"
#else
#export PS1="\[$(tput setaf 1)\]┌─╼ \[$(tput setaf 7)\][\w]\n\[$(tput setaf 1)\]\$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 1)\]└────╼\"; else echo \"\[$(tput setaf 1)\]└╼\"; fi) \[$(tput setaf 7)\]"
#fi
#
#trap 'echo -ne "\e[0m"' DEBUG

#    PS1="\[$(tput setaf 1)\]┌─╼ \[$(tput setaf 7)\][\w]\n\[$(tput setaf 1)\]\$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 1)\]└────╼\"; else echo \"\[$(tput setaf 1)\]└╼\"; fi) \[$(tput setaf 7)\]"
## }}}

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
