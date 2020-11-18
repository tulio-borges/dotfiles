function mssh
  kitty @ set-colors ~/.config/kitty/ssh-theme.conf
  ssh $argv
  kitty @ set-colors ~/.config/kitty/theme.conf
end
