## ~/.config/fish/config.fish

# env variables

set -gx EDITOR vim
set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"
set -gx BAT_THEME "Monokai Extended Light"

# setting up pyenv

status --is-interactive; and pyenv init - | source
status --is-interactive; and pyenv virtualenv-init - | source

starship init fish | source
zoxide init --cmd cd fish | source
