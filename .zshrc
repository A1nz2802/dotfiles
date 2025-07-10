# Created by newuser for 5.9
eval "$(starship init zsh)"

# Zsh Plugings
# --------------------------------------------------
#

# Zsh Autosuggestions Plugin
if [ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
	source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# Zsh Syntax Hightlighting Plugin
if [ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Zsh Sudo Plugin
if [ -f /usr/share/zsh-sudo/sudo.plugin.zsh ]; then
	source /usr/share/zsh-sudo/sudo.plugin.zsh
fi

# Zsh Copypath Plugin
if [ -f /home/a1nz/.local/bin/copypath ]; then
	source /home/a1nz/.local/bin/copypath
fi

# Zsh Copyfile Plugin
if [ -f /home/a1nz/.local/bin/copyfile ]; then
	source /home/a1nz/.local/bin/copyfile
fi

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt histignorealldups sharehistory

# Use modern completion system
autoload -Uz compinit
compinit
 
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
 
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Custom Functions
# -----------------------------------------------
# 

# Set Target

function settarget(){
    ip_address=$1
    machine_name=$2
    echo "$ip_address $machine_name" > /home/a1nz/.local/bin/target
}

# Clear Target

function cleartarget(){
    echo '' > /home/a1nz/.local/bin/target
}

# Better Path

function bpath() {
    local GREEN='\033[0;32m'
    local RED='\033[0;31m'
    local NC='\033[0m' # Sin color (para resetear)
    local i=1

    for dir in $path; do
        if [ -d "$dir" ]; then
            printf "%3s. ${GREEN}✔ %s${NC}\n" "$i" "$dir"
        else
            printf "%3s. ${RED}✖ %s${NC}\n" "$i" "$dir"
        fi
        ((i++))
    done
}

# Custom Aliases
# -----------------------------------------------
#

# bat
alias cat='bat'
alias catn='bat --style=plain'
alias catnp='bat --style=plain --paging=never'
 
# ls
alias ll='lsd -lh --group-dirs=first'
alias la='lsd -a --group-dirs=first'
alias l='lsd --group-dirs=first'
alias lla='lsd -lha --group-dirs=first'
alias ls='lsd --group-dirs=first'

# kitty image
alias icat='kitty +kitten icat'

# Keybindings
bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
