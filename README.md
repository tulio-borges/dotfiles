# Tulio's dot and conf files

Hello and welcome everybody! Here you have my configuration files. A summary of what you'll find:

- [xmonad](https://github.com/xmonad/xmonad) config and .xprofile
  - My display manager of choice is LightDM with the [Litarvan greeter](https://github.com/Litarvan/lightdm-webkit-theme-litarvan)
- [polybar](https://github.com/polybar/polybar) modules and configurations
- [Kitty](https://github.com/kovidgoyal/kitty) config
- [Fish shell](https://github.com/fish-shell/fish-shell) config and functions
- [rofi](https://github.com/davatorium/rofi) styles
- My .gitconfig
- Convenience inputrc
- Background images (sources: [1](https://drive.google.com/drive/folders/1o1qjRgkJtnF_8uGB1z6MRsQUjWinHUsw))

Packages needed for some of these configs to work:

- picom (X11 compositor) feh (to set the background image)
- hacksaw xclip shotgun (for screen shots)
- starship exa bat git-delta (CLI utilities)
- playerctl zscroll-git (for the spotify module on polybar)
- ttf-font-awesome ttf-material-design-icons nerd-fonts-noto nerd-fonts-fira-code (fonts)

## How to get started with these configs

My suggestion would be to slowly cherry pick pieces of the configurations you like and try them out :)

For xmonad + polybar you can check them out of a fresh system using a VM. Be sure to understand/customize the xmonad keybindings before importing these configurations.

## Note

that some of these configurations are very specific for my system, such as the polybar pulseaudio modules, so make sure to pay attention to your logs to figure out if there is anything broken.

## Feedback

[Suggestions](https://github.com/tulio-borges/dotfiles/issues) are always welcome!
