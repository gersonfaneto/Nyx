import Data.Map qualified as Map
import Data.Maybe qualified as Maybe

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap  -- <<< NEW
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
import XMonad.StackSet qualified as StackSet
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import System.IO (writeFile)        -- <<< NEW
import qualified Data.Map as M       -- <<< NEW

import Text.Regex.Posix ((=~))

------------------------------------------------------------------------
-- SUBMAP INDICATOR HELPERS
------------------------------------------------------------------------

setSubmap :: String -> X ()
setSubmap name = io $ writeFile "/home/gerson/.cache/xmonad-submap" name

namedSubmap :: String -> M.Map (KeyMask, KeySym) (X ()) -> X ()
namedSubmap name mp = do
    setSubmap name
    submap mp
    setSubmap ""

------------------------------------------------------------------------

q ~? x = fmap (=~ x) q

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

mFloat :: ManageHook
mFloat = doRectFloat (StackSet.RationalRect 0.15 0.15 0.7 0.7)

mManageHook =
    composeAll
        [ className =? "Navigator" --> doFloat
        , className =? "Dunst" --> doFloat
        , className =? "wiremix" --> mFloat
        , className =? "firefox" --> doShift "1"
        , className =? "Alacritty" --> doShift "2"
        , className =? "Emacs" --> doShift "10"
        , className ~? "^Minecraft*" --> doShift "5"
        , className =? "PrismLauncher" --> mFloat <> doShift "5"
        ]
        <+> insertPosition Below Newer

myStartupHook :: X ()
myStartupHook = do
    spawn "setxkbmap -layout br -variant thinkpad &"
    spawn "wallpaper --default &"
    spawn "background dark &"
    spawn "battery -L 20 -n &"
    spawn "xset s 600 &"
    spawn "xss-lock -l -- xsecurelock &"
    spawn "killall -q dunst; dunst -config $HOME/.config/dunst/dunstrc &"
    spawn "xsetroot -cursor_name left_ptr &"
    io $ writeFile "/home/gerson/.cache/xmonad-submap" ""   -- <<< NEW

------------------------------------------------------------------------
-- SUBMAPS
------------------------------------------------------------------------

openSubmap = namedSubmap "Open" $ M.fromList
    [ ((0, xK_e), spawn mEditor)
    , ((0, xK_b), spawn mBrowser)
    ]

rofiSubmap = namedSubmap "Rofi" $ M.fromList
    [ ((0, xK_r), spawn "rofi-run")
    , ((0, xK_a), spawn "rofi-apps")
    , ((0, xK_m), spawn "rofi-music")
    , ((0, xK_s), spawn "rofi-audio")
    , ((0, xK_p), spawn "rofi-capture")
    ]

systemSubmap = namedSubmap "System" $ M.fromList
    [ ((0, xK_c), spawn "caffeine")
    , ((0, xK_s), spawn "silence")
    , ((0, xK_d), spawn "dunstctl close-all")
    , ((0, xK_x), spawn "xmonad --recompile && xmonad --restart")
    ]

mediaSubmap = namedSubmap "Media" $ M.fromList
    [ ((0, xK_k), spawn "volume up")
    , ((0, xK_j), spawn "volume down")
    , ((0, xK_m), spawn "volume mute")
    , ((0, xK_v), spawn "volume mute")
    , ((0, xK_p), spawn "playerctl play-pause")
    , ((0, xK_h), spawn "playerctl previous")
    , ((0, xK_l), spawn "playerctl next")
    , ((0, xK_w), spawn "alacritty --class 'wiremix' --command 'wiremix'")
    ]

------------------------------------------------------------------------

mKeys =
    [ ("M-<Escape>", spawn "rofi-system")
    , ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')

      -- SUBMAP prefixes:
    , ("M-o", openSubmap)
    , ("M-r", rofiSubmap)
    , ("M-s", systemSubmap)
    , ("M-m", mediaSubmap)

    , ("M-t z", spawn "boomer")
    , ("M-<F1>", spawn "wallpaper --select")
    , ("M-S-<F1>", spawn "wallpaper --default")
    , ("M-<F2>", spawn "background alt")
    , ("M-q", kill)
    , ("M-C-l", nextWS)
    , ("M-C-h", prevWS)
    , ("M-S-l", shiftToNext >> nextWS)
    , ("M-S-h", shiftToPrev >> prevWS)
    , ("M-<Tab>", toggleWS)
    , ("M-j", windows StackSet.focusDown)
    , ("M-k", windows StackSet.focusUp)
    , ("M-,", sendMessage NextLayout)
    , ("M-S-<Space>", withFocused toggleFloat)
    , ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-S-a", setWindowSpacing (Border 3 3 3 3) >> setScreenSpacing (Border 3 3 3 3))
    ]
    ++ [ (mask ++ "M-" ++ [key], windows $ action tag)
       | (tag, key) <- zip mWorkspaces "1234567890"
       , (action, mask) <- [(StackSet.greedyView, ""), (StackSet.shift, "S-")]
       ]

------------------------------------------------------------------------

toggleFloat w =
    windows
        ( \s ->
            if Map.member w (StackSet.floating s)
                then StackSet.sink w s
                else StackSet.float w (StackSet.RationalRect 0.15 0.15 0.7 0.7) s
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
