import Data.Map qualified as M

import XMonad
import XMonad.Actions.CycleWS
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

cBackground = "#002B36"
cForeground = "#657B83"
cBlack = "#32344A"
cBlack' = "#444B6A"
cRed = "#DC322F"
cGreen = "#859900"
cYellow = "#B58900"
cBlue = "#268BD2"
cMagenta = "#D33682"
cCyan = "#2AA198"

mSpacing = spacingWithEdge 3

mWorkspaces :: [String]
mWorkspaces = map show [1 .. 10]

mModMask :: KeyMask
mModMask = mod4Mask

mTerminal :: String
mTerminal = "alacritty"

mTerminal' :: String
mTerminal' = "ghostty"

mEditor :: String
mEditor = "emacsclient --create-frame --alternate-editor 'emacs'"

mBrowser :: String
mBrowser = "firefox"

mLayoutHook =
    avoidStruts $
        renamed [Replace "Tall"] (mSpacing tall)
            ||| renamed [Replace "Wide"] (mSpacing (Mirror tall))
            ||| renamed [Replace "Full"] (mSpacing Full)
            ||| renamed [Replace "Spiral"] (mSpacing (spiral (6 / 7)))
  where
    tall = ResizableTall 1 (3 / 100) (11 / 20) []

mManageHook =
    composeAll
        [ className =? "Navigator" --> doFloat
        , className =? "Dunst" --> doFloat
        , className =? "wiremix" --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)
        -- , className =? "firefox" --> doShift "1"
        -- , className =? "Alacritty" --> doShift "2"
        -- , className =? "Emacs" --> doShift "10"
        ]
        <+> insertPosition Below Newer

myStartupHook :: X ()
myStartupHook = do
    spawn "setxkbmap -layout br -variant thinkpad &"
    spawn "wallpaper &"
    spawn "background dark &"
    spawn "battery -L 20 -n &"
    spawn "xset s 600 &"
    spawn "xss-lock -l -- xsecurelock &"
    spawn "killall -q dunst; dunst -config $HOME/.config/dunst/dunstrc &"
    spawn "xsetroot -cursor_name left_ptr &"

mKeys =
    [ ("M-<Escape>", spawn "rofi-system")
    , ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')
    , ("M-<Space> o e", spawn mEditor)
    , ("M-<Space> o b", spawn mBrowser)
    , ("M-<Space> r r", spawn "rofi-run")
    , ("M-<Space> r a", spawn "rofi-apps")
    , ("M-<Space> r m", spawn "rofi-music")
    , ("M-<Space> r v", spawn "rofi-audio")
    , ("M-<Space> r s", spawn "rofi-capture")
    , ("M-s c t", spawn "caffeine")
    , ("M-s n t", spawn "silence")
    , ("M-s n c", spawn "dunstctl close-all")
    , ("M-s x r", spawn "xmonad --recompile && xmonad --restart")
    , ("M-t z", spawn "boomer")
    , ("M-q", kill)
    , ("M-C-l", nextWS)
    , ("M-C-h", prevWS)
    , ("M-S-l", shiftToNext >> nextWS)
    , ("M-S-h", shiftToPrev >> prevWS)
    , ("M-<Tab>", toggleWS)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-,", sendMessage NextLayout)
    , ("M-S-<Space>", withFocused toggleFloat)
    , ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-S-a", setWindowSpacing (Border 3 3 3 3) >> setScreenSpacing (Border 3 3 3 3))
    , ("M-<Space> 1", windows $ W.greedyView "1")
    , ("M-<Space> 2", windows $ W.greedyView "2")
    , ("M-<Space> 3", windows $ W.greedyView "3")
    , ("M-<Space> 4", windows $ W.greedyView "4")
    , ("M-<Space> 5", windows $ W.greedyView "5")
    , ("M-<Space> 6", windows $ W.greedyView "6")
    , ("M-<Space> 7", windows $ W.greedyView "7")
    , ("M-<Space> 8", windows $ W.greedyView "8")
    , ("M-<Space> 9", windows $ W.greedyView "9")
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
           , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
           ]

toggleFloat w =
    windows
        ( \s ->
            if M.member w (W.floating s)
                then W.sink w s
                else W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s
        )

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

myConfig =
    def
        { modMask = mModMask
        , terminal = mTerminal
        , workspaces = mWorkspaces
        , borderWidth = 2
        , normalBorderColor = cBlack
        , focusedBorderColor = cMagenta
        , startupHook = myStartupHook
        , layoutHook = mLayoutHook
        , manageHook = mManageHook <+> manageDocks
        }
        `additionalKeysP` mKeys

mXmobar = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB mXmobar defToggleStrutsKey $ myConfig
