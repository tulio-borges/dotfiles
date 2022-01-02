{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Polybar where

import Control.Monad (forM_, join)
import Data.Function (on)
import XMonad.Util.NamedWindows (getName)

-- This module sets up the infrastructure on Xmonad to use the Polybar xmonad
-- modules.
-- I've replaced this module with the XMonad.Hooks.EwmhDesktops contrib package
-- to add support for EWM Hints on xmonad. If you'd rather use this approach you
-- can remove the mentioned package and import this instead.
-- To use this package call setLogFiles on the haskell main and add
-- polybarLogHook to your logHook

-- Using named pipes to log updates for polybar
polybarLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (crop title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
  where
    fmt currWs ws
      | currWs == ws = " %{o#b8bb26}%{+o} %{F#b8bb26}" ++ ws ++ "%{F-} %{o-}%{-o} "
      | otherwise = "  " ++ ws ++ "  "
    sort' = sortBy (compare `on` (!! 0))
    crop xs
      | length xs >= 50 = take 47 xs ++ "..."
      | otherwise = xs

-- Event Log Hook using named pipes
setLogFiles = forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
  safeSpawn "mkfifo" ["/tmp/" ++ file]