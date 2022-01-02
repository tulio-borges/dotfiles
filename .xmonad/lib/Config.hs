{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config where

import qualified Data.Map as M
import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback
import XMonad.Actions.Navigation2D
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W

-- import Control.Monad (join)
import Data.List (length, sortBy, take)
-- import Data.Function (on)
-- import XMonad.Util.NamedWindows (getName)


modMask' :: KeyMask
modMask' = mod4Mask

delta :: Rational
delta = 3 / 100

black = "#212121"
green = "#24bf2b"

myLayouts =
  avoidStruts
    . maximize
    . minimize
    . spacingRaw True (Border 0 0 0 0) False (Border 10 10 10 10) True
    . gaps [(L, 10), (R, 10)]
    . B.boringWindows
    $ myTall ||| Mirror myTall ||| noBorders Full
    where myTall = smartBorders $ Tall 1 (1 / 100) (1 / 2)

switchWorkspaceToWindow :: Window -> X ()
switchWorkspaceToWindow w = windows $ do
  tag <- W.currentTag
  W.focusWindow w . W.greedyView tag . W.focusWindow w

workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myManageHook =
  composeAll
    [ className =? "Peek" --> doFloat,
      className =? "feh" --> doFloat,
      -- Used by Chromium developer tools, maybe other apps as well
      role =? "pop-up" --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [isFullscreen -?> doFullFloat]
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                then W.sink w s
                else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf),
      ((modm, xK_r), spawn "rofi -show drun"),
      ((modm, xK_Tab), spawn "rofi -show window"),
      -- Lock the screen and display the greeter
      ((modm, xK_l), spawn "dm-tool switch-to-greeter"),
      -- Print Screen to clipboard
      ((noModMask, xK_Print), spawn "shotgun - | xclip -t 'image/png' -selection clipboard"),
      -- Make selection and then print to clipboard
      ((modm .|. shiftMask, xK_s), spawn "$HOME/.xmonad/scripts/print-selection.sh"),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      -- Reset to default layout (currently Tall)
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      -- Move focus to the next window
      ((modm, xK_j), B.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), B.focusUp),
      -- Move focus to the master window
      ((modm, xK_m), B.focusMaster),
      -- Basic ciclying actions
      -- Move to next Workspace
      ((modm, xK_Up), nextWS),
      -- Move to previous Workspace
      ((modm, xK_Down), prevWS),
      -- Move window to next Workspace
      ((modm .|. shiftMask, xK_Up), shiftToNext >> nextWS),
      -- Move window to previous Workspace
      ((modm .|. shiftMask, xK_Down), shiftToPrev >> prevWS),
      -- Move to next Screen
      ((modm, xK_Left), nextScreen),
      -- Move to previous Screen
      ((modm, xK_Right), prevScreen),
      -- Move window to next Screen
      ((modm .|. shiftMask, xK_Left), shiftNextScreen),
      -- Move window to previous Screen
      ((modm .|. shiftMask, xK_Right), shiftPrevScreen),
      -- Swap the focused window with the master window
      ((modm .|. shiftMask, xK_m), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modm, xK_u), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_i), sendMessage Expand),
      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink),
      -- Push window to floating
      ((modm, xK_f), withFocused toggleFloat),
      -- Quit xmonad (personal preference to kill instead of exiting xmonad)
      ((modm .|. shiftMask .|. controlMask, xK_q), io exitSuccess),
      -- Kill current window application
      ((modm .|. shiftMask, xK_q), kill),
      -- Restart xmonad
      ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart"),
      -- 2D navigation
      ((modm .|. shiftMask, xK_l), screenGo R True),
      ((modm .|. shiftMask, xK_h), screenGo L True),
      ((modm .|. controlMask, xK_l), screenSwap R True),
      ((modm .|. controlMask, xK_h), screenSwap L True),
      -- Struts and Gaps...
      ((modm .|. controlMask, xK_0), sendSomeMessages [sm ToggleStruts, sm ToggleGaps]),
      -- Toggle polybar
      -- Sadly polybar can't be controled individually per workspace
      -- I also didn't figure out how to toggle polybar and struts/gaps simultaniously
      ((modm .|. controlMask, xK_9), spawn "polybar-msg cmd toggle")
    ]
      ++
      -- Media hotkeys
      [ ((mod5Mask, k), spawn $ "playerctl " ++ m)
        | (m, k) <- zip ["previous", "play-pause", "next"] [xK_3 .. xK_5]
      ]
      ++ [ ((noModMask, k), spawn $ "playerctl " ++ m)
           | (m, k) <-
               [ ("previous", xF86XK_AudioPrev),
                 ("play-pause", xF86XK_AudioPlay),
                 ("next", xF86XK_AudioNext)
               ]
         ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

-- Using named pipes to log updates for polybar
-- Uncomment these lines if using the xmonad modules on Polybar
-- Remember to uncomment the imports as well!
-- myLogHook = do
--   winset <- gets windowset
--   title <- maybe (return "") (fmap show . getName) . W.peek $ winset
--   let currWs = W.currentTag winset
--   let wss = map W.tag $ W.workspaces winset
--   let wsStr = join $ map (fmt currWs) $ sort' wss

--   io $ appendFile "/tmp/.xmonad-title-log" (crop title ++ "\n")
--   io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

--   where fmt currWs ws
--           | currWs == ws = " %{o#b8bb26}%{+o} %{F#b8bb26}" ++ ws ++ "%{F-} %{o-}%{-o} "
--           | otherwise    = "  " ++ ws ++ "  "
--         sort' = sortBy (compare `on` (!! 0))
--         crop xs
--           | length xs >= 50 = take 47 xs ++ "..."
--           | otherwise       = xs

myConfig =
  def
    { terminal = "kitty",
      layoutHook = myLayouts,
      manageHook =
        placeHook (smart (0.5, 0.5))
          <+> manageDocks
          <+> myManageHook
          <+> myManageHook'
          <+> manageHook def,
      handleEventHook =
        docksEventHook
          <+> minimizeEventHook
          <+> fullscreenEventHook,
      keys = myKeys,
      focusFollowsMouse = False,
      clickJustFocuses = False,
      borderWidth = 2,
      normalBorderColor = black,
      focusedBorderColor = green,
      workspaces = workspaces',
      modMask = modMask'
    }
