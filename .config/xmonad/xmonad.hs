import Data.Map qualified as M

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- TokyoNight Colors
cBackground = "#1a1b26" -- background
cForeground = "#a9b1d6" -- foreground
cBlack = "#32344a" -- black
cBlack' = "#444b6a" -- bright black
cRed = "#f7768e" -- red
cGreen = "#9ece6a" -- green
cYellow = "#e0af68" -- yellow
cBlue = "#7aa2f7" -- blue
cMagenta = "#ad8ee6" -- magenta
cCyan = "#0db9d7" -- cyan

-- Appearance
mBorderWidth = 2

mNormalBorderColor = cBlack'

mFocusedBorderColor = cMagenta

-- Gaps (matching dwm: 3px all around)
mySpacing = spacingWithEdge 3

-- Workspaces
mWorkspaces = map show [1 .. 10]

-- Mod key (Super/Windows key)
mModMask = mod4Mask

-- Terminal
mTerminal = "alacritty"
mTerminal' = "ghostty"

-- Layouts
mLayoutHook =
    avoidStruts $
        renamed [Replace "Tall"] (mySpacing tall)
            ||| renamed [Replace "Wide"] (mySpacing (Mirror tall))
            ||| renamed [Replace "Full"] (mySpacing Full)
            ||| renamed [Replace "Spiral"] (mySpacing (spiral (6 / 7)))
  where
    tall = ResizableTall 1 (3 / 100) (11 / 20) []

-- Window rules (matching dwm config)
mManageHook =
    composeAll
        [ className =? "Navigator" --> doFloat
        , className =? "Dunst" --> doFloat
        , className =? "wiremix" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)
        , className =? "firefox" --> doShift "1"
        , className =? "Alacritty" --> doShift "2"
        , className =? "Emacs" --> doShift "10"
        ]
        <+> insertPosition Below Newer

-- Key bindings (matching dwm as closely as possible)
mKeys =
    -- Launch applications
    [ ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')
    , ("M-e", spawn $ mTerminal <> " --command 'nvim'")
    , ("M-S-e", spawn "emacsclient --create-frame --alternate-editor 'emacs'")
    , ("M-<Backspace>", spawn "rofi-system")
    , ("M-d", spawn "rofi-run")
    , ("M-M1-d", spawn "rofi-apps")
    , ("M-m", spawn "rofi-music")
    , ("M-S-m", spawn "rofi-audio")
    , ("M-p", spawn "rofi-capture")
    , ("M-n", spawn "silence")
    , ("M-c", spawn "caffeine")
    , ("M-S-n", spawn "dunstctl close-all")
    , ("M-z", spawn "boomer")
    , -- Window management
      ("M-q", kill)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-<Tab>", windows W.focusDown)
    , -- , -- Master area
      --   ("M-h", sendMessage Expand)
      -- , ("M-g", sendMessage Shrink)
      -- , ("M-i", sendMessage (IncMasterN 1))
      -- , ("M-p", sendMessage (IncMasterN (-1)))
      -- , -- Layout switching
      --   ("M-t", sendMessage $ JumpToLayout "Tall")
      -- , ("M-f", sendMessage $ JumpToLayout "Full")
      -- , ("M-c", sendMessage $ JumpToLayout "Spiral")
      ("M-S-,", sendMessage NextLayout)
    , -- , ("M-n", sendMessage NextLayout)
      -- Floating
      ("M-S-<Space>", withFocused toggleFloat)
    , -- , -- Gaps (z to increase, x to decrease, a to toggle)
      --   ("M-z", incWindowSpacing 8)
      -- , ("M-x", decWindowSpacing 8)
      ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-S-a", setWindowSpacing (Border 3 3 3 3) >> setScreenSpacing (Border 3 3 3 3))
    , -- Quit/Restart
      ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
    , -- Keychords for tag navigation (Mod+Space then number)
      ("M-<Space> 1", windows $ W.greedyView "1")
    , ("M-<Space> 2", windows $ W.greedyView "2")
    , ("M-<Space> 3", windows $ W.greedyView "3")
    , ("M-<Space> 4", windows $ W.greedyView "4")
    , ("M-<Space> 5", windows $ W.greedyView "5")
    , ("M-<Space> 6", windows $ W.greedyView "6")
    , ("M-<Space> 7", windows $ W.greedyView "7")
    , ("M-<Space> 8", windows $ W.greedyView "8")
    , ("M-<Space> 9", windows $ W.greedyView "9")
    , ("M-<Space> f", spawn "firefox")
    , ("M-<Space> S-f", spawn "firefox --private-window")
    , -- Volume controls
      ("<XF86AudioRaiseVolume>", spawn "volume up")
    , ("<XF86AudioLowerVolume>", spawn "volume down")
    , ("<XF86AudioMute>", spawn "volume mute")
    , ("M-<XF86AudioMute>", spawn "volume mute")
    , ("M-M1-k", spawn "volume up")
    , ("M-M1-j", spawn "volume down")
    , ("M-M1-m", spawn "volume mute")
    , ("M-M1-v", spawn "volume mute")
    , -- Playback control
      ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("M-M1-h", spawn "playerctl previous")
    , ("M-M1-l", spawn "playerctl next")
    , ("M-M1-p", spawn "playerctl play-pause")
    , -- Brightness control
      ("<XF86MonBrightnessDown>", spawn "brightness down")
    , ("<XF86MonBrightnessUp>", spawn "brightness up")
    ]
        ++
        -- Standard TAGKEYS behavior (Mod+# to view, Mod+Shift+# to move)
        [ (mask ++ "M-" ++ [key], windows $ action tag)
        | (tag, key) <- zip mWorkspaces "1234567890"
        , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
        ]

-- Helper function for toggling float
toggleFloat w =
    windows
        ( \s ->
            if M.member w (W.floating s)
                then W.sink w s
                else W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s
        )

-- XMobar PP (Pretty Printer) configuration
myXmobarPP :: PP
myXmobarPP =
    def
        { ppSep = xmobarColor cBlack' "" " │ "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = xmobarColor cCyan ""
        , ppHidden = xmobarColor cForeground ""
        , ppHiddenNoWindows = xmobarColor cBlack' ""
        , ppUrgent = xmobarColor cRed cYellow
        , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = wrap (xmobarColor cCyan "" "[") (xmobarColor cCyan "" "]") . xmobarColor cForeground "" . ppWindow
    formatUnfocused = wrap (xmobarColor cBlack' "" "[") (xmobarColor cBlack' "" "]") . xmobarColor cBlack' "" . ppWindow
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

-- Main configuration
myConfig =
    def
        { modMask = mModMask
        , terminal = mTerminal
        , workspaces = mWorkspaces
        , borderWidth = mBorderWidth
        , normalBorderColor = mNormalBorderColor
        , focusedBorderColor = mFocusedBorderColor
        , layoutHook = mLayoutHook
        , manageHook = mManageHook <+> manageDocks
        }
        `additionalKeysP` mKeys

-- XMobar status bar configuration
mXmobar = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB mXmobar defToggleStrutsKey $ myConfig
