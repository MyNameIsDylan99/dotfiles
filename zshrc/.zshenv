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
# export PYENV_ROOT="$HOME/.pyenv"
# [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init - bash)"

export PATH=$PATH:/usr/local/go/bin

export OPENSSL_DIR=$OPENSSL_DIR:/home/linuxbrew/.linuxbrew/Cellar/openssl@3/3.4.1/include/
. "$HOME/.cargo/env"

export GDK_SCALE=3
export GDK_DPI_SCALE=0.5

export PATH="$HOME/.emacs.d/bin/:$PATH"
