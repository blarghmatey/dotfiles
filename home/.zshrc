setopt COMPLETE_ALIASES
autoload -U select-word-style
select-word-style bash
# Aliases
alias ls='/bin/ls --indicator-style=slash --color=auto'
alias et='emacsclient -nw -t'
alias cat='/usr/sbin/bat'
alias vim='/usr/sbin/nvim'
alias gemini='npx -y @google/gemini-cli'
alias ghc='npx -y @github/copilot'
alias amp='npx -y @sourcegraph/amp'
alias kilo='npx -y @kilocode/cli'
alias prune_pods='kubectl delete pods --field-selector=status.phase!=Running --all-namespaces'
alias ccr='npx -y @musistudio/claude-code-router'

function retire_concourse_worker {
    aws ec2 describe-instances --filters Name=private-dns-name,Values=$1.ec2.internal | jq ".Reservations[0].Instances[0].InstanceId" | xargs aws ec2 terminate-instances --instance-id
}

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
# End of lines configured by zsh-newuser-install

# GitHub Copilot CLI dedicated history
# Store copilot commands in a separate history file with large capacity
if [[ -n "$GITHUB_COPILOT_CLI" ]]; then
    export HISTFILE=~/.copilot_history
    export HISTSIZE=100000
    export SAVEHIST=500000
fi

path+=('/home/tmacey/.local/bin')
path+=('/home/tmacey/.cargo/bin')
path+=('/home/tmacey/.npm_packages/bin')
path+=('/home/tmacey/go/bin')
export PATH

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust \
    mavwolverine/zsh-safe-venv-auto

zinit load atuinsh/atuin

# carapace completion
zinit ice as'program' id-as'carapace' from'gh-r' atload' \
  autoload -Uz compinit; \
  compinit; \
  source <(carapace _carapace);'
zinit light carapace-sh/carapace-bin

# Load starship theme
# line 1: `starship` binary as command, from github release
# line 2: starship setup at clone(create init.zsh, completion)
# line 3: pull behavior same as clone, source init.zsh
# zinit ice as"command" from"gh-r" \
#           atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
#           atpull"%atclone" src"init.zsh"
# zinit light starship/starship
### End of Zinit's installer chunk
export SPACESHIP_PROMPT_ASYNC=FALSE
eval "$(starship init zsh)"
#source <(carapace _carapace)

if [[ -z "$SSH_AUTH_SOCK" ]]; then
  eval "$(ssh-agent)"
fi
ssh-add ~/.ssh/id_rsa 2>/dev/null
ssh-add ~/.ssh/odl_rsa 2>/dev/null
ssh-add ~/.ssh/odl_app 2>/dev/null

# env configs
EDITOR=emacsclient
export EDITOR

# AI/Ollama
AWS_REGION=us-east-1
export AWS_REGION
OLLAMA_API_BASE="http://127.0.0.1:11434"
export OLLAMA_API_BASE
OLLAMA_BASE_URL="http://127.0.0.1:11434"
EXPERT_OLLAMA_BASE_URL="http://127.0.0.1:11434"
export OLLAMA_BASE_URL
export EXPERT_OLLAMA_BASE_URL
export OLLAMA_CUDA=1
export OLLAMA_NUM_THREADS=8

# Claude Code
# Enable Bedrock integration
# export CLAUDE_CODE_USE_BEDROCK=1
export AWS_REGION=us-east-1  # or your preferred region
# Optional: Override the region for the small/fast model (Haiku)
# export ANTHROPIC_SMALL_FAST_MODEL_AWS_REGION=us-west-2
# export CLAUDE_CODE_MAX_OUTPUT_TOKENS=4096
# export MAX_THINKING_TOKENS=1024
export OPENROUTER_API_KEY=$(pass openrouter-key)

export HF_TOKEN=$(pass huggingface-token)

# Java Setup
JAVA_HOME=/usr/lib/jvm/default
export JAVA_HOME

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Added by dbt installer
export PATH="$PATH:/home/tmacey/.local/bin"

# dbt aliases
alias dbtf=/home/tmacey/.local/bin/dbt
