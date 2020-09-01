# ~/.config/fish/config.fish

set -gx EDITOR vim
set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"
set -gx BAT_THEME "Monokai Extended Light"

starship init fish | source
