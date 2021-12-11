import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run

import Control.Monad (forM_)

import Config

main :: IO ()
main = do
  -- Event Log Hook using named pipes
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    safeSpawn "mkfifo" ["/tmp/" ++ file]

  xmonad . ewmh . withUrgencyHook NoUrgencyHook
    . withNavigation2DConfig def
    $ myConfig
      { logHook = myLogHook,
        startupHook = spawn "$HOME/.config/polybar/launch.sh"
      }
