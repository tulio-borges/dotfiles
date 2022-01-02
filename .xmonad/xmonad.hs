import Config
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run

main :: IO ()
main = do
  xmonad . ewmh . withUrgencyHook NoUrgencyHook
    . withNavigation2DConfig def
    $ myConfig
