# Flutter binaries
export PATH="$HOME/development/flutter/bin:$PATH"

# Cargo binaries
export PATH="$HOME/.cargo/bin:$PATH"

# Add android SDK path variables
export ANDROID_HOME=~/Android/Sdk
export PATH="$ANDROID_HOME/cmdline-tools/latest/bin:$PATH"

# Environment variable for nvim
export EDITOR=nvim

# Python
export PYTHONHOME=/usr

# Nerd dictation
export PATH=$PATH:/home/dylan/Dokumente/GameDev/Git/Unity/nerd-dictation

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - bash)"

export PATH=$PATH:/usr/local/go/bin
. "$HOME/.cargo/env"
