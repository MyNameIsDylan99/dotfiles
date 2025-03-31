# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

autoload -U +X bashcompinit && bashcompinit
autoload -U +X compinit && compinit

#source /share/powerlevel10k/powerlevel10k.zsh-theme
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# history setup
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

# Don't output reminder / error when using exec zsh directly
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

# Add homebrew to path
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# Plugins
source /home/linuxbrew/.linuxbrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /home/linuxbrew/.linuxbrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=250"

# ---- Eza (better ls) -----
alias ls="eza --icons=always"

# ---- Zoxide (better cd) ----
eval "$(zoxide init zsh)"
alias cd="z"

# Set up fzf key bindings and fuzzy completion
#source <(fzf --zsh)


# Setup vi editing
bindkey -v

# Lets you open yazi with y
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

# Lets you open neovim with nv
alias -g nv='nvim'
# using G for piping output to less
alias -g G='| grep -i'
# using L for piping output to less
alias -g L='| less'

# Using fs to search the current directory recursively for string matches and outputs the directory
alias -g fs='rg --no-heading --line-number --color=always "" . | fzf --ansi --delimiter : --nth 3.. --preview "batcat --color=always --style=numbers {1} --highlight-line {2}" | awk -F: '\''{print $1}'\'''


# Change to obsidian directory and open neovim
alias obsi='cd ~/Dokumente/Programme/Obsidian; nvim'
# search obsidian folder for file match
alias of='nvim "$(fzf --walker-root=/home/dylan/Dokumente/Programme/Obsidian)"'
# search obsidian folder for string match
alias os='rg --no-heading --line-number --color=always "" ~/Dokumente/Programme/Obsidian | fzf --ansi --delimiter : --nth 3.. --preview "batcat --color=always --style=numbers {1} --highlight-line {2}" | awk -F: '\''{print $1, $2}'\'' | sed "s/\(.*\) \(.*\)/nvim +\2 \"\1\"/" | zsh'
# Open musikcube
alias mc='musikcube'
# Open lazygit
alias lg='lazygit'
# Change directory to Wish of Hers directory and start neovim
alias wof='cd ~/Dokumente/Git/Unity/WishOfHers;nvim'


#when no other command is used cd is assumed to be the command
setopt autocd
# spelling correction for commands
setopt correct
# spelling correction for all arguments
setopt correctall


# ZSH-Vi-mode
source /home/linuxbrew/.linuxbrew/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
ZVM_VI_INSERT_ESCAPE_BINDKEY=jj

# Completion using arrow keys (based on history)
bindkey '^K' history-search-backward
bindkey '^J' history-search-forward
bindkey '^O' autosuggest-accept

eval "$(pyenv virtualenv-init -)"
