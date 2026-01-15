{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module XMonad.Custom.Config
  ( mConfig
  )
where

import           Flow
import           System.IO                           (writeFile)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Submap
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doRectFloat, isDialog)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Util.EZConfig                (additionalKeysP)
import           XMonad.Util.Loggers
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties
import           XMonad.Util.WorkspaceCompare

import qualified Data.Map                            as M
import qualified XMonad.StackSet                     as S

import           XMonad.Custom.Scratchpads

import qualified XMonad.Custom.Hooks.XMobar          as C
import qualified XMonad.Custom.Manage.ManageHook     as C
import qualified XMonad.Custom.Theme                 as C

mWorkspaces :: [String]
mWorkspaces = map show [1 .. 10]

mModMask :: KeyMask
mModMask = mod4Mask

mTerminal :: String
mTerminal = "ghostty"

mTerminal' :: String
mTerminal' = "alacritty"

mEditor :: String
mEditor = "emacsclient --create-frame --alternate-editor 'emacs'"

mSystem :: String
mSystem = "alacritty --class btm --command btm"

mMusic :: String
mMusic = "alacritty --class kew --command kew"

mSound :: String
mSound = "alacritty --class wiremix --command wiremix"

mBrowser :: String
mBrowser = "qutebrowser"

mFiles :: String
mFiles = "nautilus"

mCalc :: String
mCalc = "qalculate-gtk"

mLayoutHook =
  avoidStruts $
    smartBorders $
      renamed [Replace "Tall"] (mSpacing tall)
        ||| renamed [Replace "Wide"] (mSpacing (Mirror tall))
        ||| renamed [Replace "Full"] (mSpacing Full)
        ||| renamed [Replace "Spiral"] (mSpacing (spiral (6 / 7)))
  where
    tall = ResizableTall 1 (3 / 100) (11 / 20) []
    mSpacing = spacingWithEdge 8

myStartupHook :: X ()
myStartupHook = do
  spawn "pkill dunst; dunst -config $HOME/.config/dunst/dunstrc &"
  spawn "feh --bg-fill $HOME/.local/share/wallpapers/1920x1080.jpg &"
  spawn "battery -L 20 -n &"
  spawn "xrandr --output HDMI-1 --same-as eDP-1 &"
  spawn "xset s 600 &"
  spawn "xss-lock -l -- xsecurelock &"
  spawn "xsetroot -cursor_name left_ptr &"
  spawn "setxkbmap -option ctrl:nocaps -layout br -variant thinkpad &"
  io $ writeFile "/home/gerson/.cache/xmonad-submap" ""

setSubmap :: String -> X ()
setSubmap name = io $ writeFile "/home/gerson/.cache/xmonad-submap" name

namedSubmap :: String -> M.Map (KeyMask, KeySym) (X ()) -> X ()
namedSubmap name mp = do
  setSubmap name
  submap mp
  setSubmap ""

gameSubmap =
  namedSubmap "Game" $
    M.fromList
      [ ((0, xK_m), spawn "prismlauncher")
      ]

toolSubmap =
  namedSubmap "Tool" $
    M.fromList
      [ ((0, xK_z), spawn "boomer")
      , ((0, xK_m), spawn mSystem)
      , ((0, xK_s), spawn mSound)
      ]

openSubmap =
  namedSubmap "Open" $
    M.fromList
      [ ((0, xK_e), spawn mEditor)
      , ((0, xK_b), spawn mBrowser)
      , ((0, xK_m), spawn mMusic)
      , ((0, xK_c), spawn mCalc)
      , ((0, xK_f), spawn mFiles)
      , ((0, xK_p), spawn "zathura")
      ]

systemSubmap =
  namedSubmap "System" $
    M.fromList
      [ ((0, xK_s), spawn "silence")
      , ((0, xK_c), spawn "caffeine")
      , ((0, xK_b), spawn "background alt")
      , ((0, xK_d), spawn "dunstctl close-all")
      , ((0, xK_x), spawn "xmonad --recompile && xmonad --restart")
      ]

workspaceSubmap =
  namedSubmap "Workspace" $
    M.fromList
      [ ((0, xK_t), sendMessage $ JumpToLayout "Tall")
      , ((0, xK_w), sendMessage $ JumpToLayout "Wide")
      , ((0, xK_f), sendMessage $ JumpToLayout "Full")
      , ((0, xK_s), sendMessage $ JumpToLayout "Spiral")
      ]

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS =
  findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= windows <. S.view
prevNonEmptyWS =
  findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= windows <. S.view

mKeys =
  [ ("M-<Escape>", spawn "system")
  , ("M-<Space>", spawn "launcher")
  , ("M-<Print>", spawn "capture")
  , ("M-<Return>", spawn mTerminal)
  , ("M-S-<Return>", spawn mTerminal')
  , ("M-g", gameSubmap)
  , ("M-t", toolSubmap)
  , ("M-o", openSubmap)
  , ("M-s", systemSubmap)
  , ("M-w", workspaceSubmap)
  , ("M-q", kill)
  , ("M-C-l", nextWS)
  , ("M-C-h", prevWS)
  , ("M-S-l", shiftToNext >> nextWS)
  , ("M-S-h", shiftToPrev >> prevWS)
  , ("M-<Tab>", toggleWS)
  , ("M-j", windows S.focusDown)
  , ("M-k", windows S.focusUp)
  , ("M-f", sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL])
  , ("M-,", prevNonEmptyWS)
  , ("M-.", nextNonEmptyWS)
  , ("M-;", spawn "rofimoji")
  , ("M-S-<Space>", withFocused toggleFloat)
  , ("M-z", incWindowSpacing 8)
  , ("M-x", decWindowSpacing 8)
  , ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
  , ("M-S-a", setWindowSpacing (Border 8 8 8 8) >> setScreenSpacing (Border 8 8 8 8))
  , ("<XF86AudioRaiseVolume>", spawn "volume up")
  , ("<XF86AudioLowerVolume>", spawn "volume down")
  , ("<XF86AudioMute>", spawn "volume mute")
  , ("M-<XF86AudioMute>", spawn "volume mute")
  , ("M-M1-k", spawn "volume up")
  , ("M-M1-j", spawn "volume down")
  , ("M-M1-m", spawn "volume mute")
  , ("M-M1-v", spawn "volume mute")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("M-M1-h", spawn "playerctl previous")
  , ("M-M1-l", spawn "playerctl next")
  , ("M-M1-p", spawn "playerctl play-pause")
  , ("<XF86MonBrightnessDown>", spawn "brightness down")
  , ("<XF86MonBrightnessUp>", spawn "brightness up")
  ]
    ++ [ (mask ++ "M-" ++ [key], windows $ action tag)
       | (tag, key) <- zip mWorkspaces "1234567890"
       , (action, mask) <- [(S.greedyView, ""), (S.shift, "S-")]
       ]

toggleFloat w =
  windows
    ( \s ->
        if M.member w (S.floating s)
          then S.sink w s
          else S.float w (S.RationalRect 0.15 0.15 0.7 0.7) s
    )

mConfig =
  def
    { modMask = mModMask
    , terminal = mTerminal
    , workspaces = mWorkspaces
    , borderWidth = 2
    , normalBorderColor = C.colorN
    , focusedBorderColor = C.colorF
    , startupHook = myStartupHook
    , layoutHook = mLayoutHook
    , manageHook = C.manageHook
    }
    `additionalKeysP` mKeys
    |> dynamicSBs C.barSpawner
    |> docks
    |> ewmh
    |> ewmhFullscreen
    |> (return :: a -> IO a)
