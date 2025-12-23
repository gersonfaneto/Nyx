import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Submap
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers   (doCenterFloat, doRectFloat,
                                               isDialog)
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import qualified XMonad.StackSet              as StackSet
import           XMonad.Util.EZConfig         (additionalKeysP)
import           XMonad.Util.Loggers
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties

import qualified Data.Map                     as Map
import qualified Data.Maybe                   as Maybe

import           System.IO                    (writeFile)

import           Text.Regex.Posix             ((=~))

q ~? x = fmap (=~ x) q

cBackground = "#002B36"
cForeground = "#657B83"
cBlack      = "#32344A"
cBlack'     = "#444B6A"
cRed        = "#DC322F"
cGreen      = "#859900"
cYellow     = "#B58900"
cBlue       = "#268BD2"
cMagenta    = "#D33682"
cCyan       = "#2AA198"

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

mFloat :: ManageHook
mFloat = doRectFloat (StackSet.RationalRect 0.15 0.15 0.7 0.7)

mManageHook =
    composeAll
        [ className =? "wiremix" --> mFloat
        , className =? "Qalculate-gtk" --> mFloat
        , isDialog --> doFloat
        ]
        <+> insertPosition Below Newer

myStartupHook :: X ()
myStartupHook = do
    spawn "setxkbmap -layout br -variant thinkpad &"
    spawn "wallpaper --default &"
    -- spawn "background dark &"
    spawn "battery -L 20 -n &"
    spawn "xset s 600 &"
    spawn "xss-lock -l -- xsecurelock &"
    spawn "killall -q dunst; dunst -config $HOME/.config/dunst/dunstrc &"
    spawn "xsetroot -cursor_name left_ptr &"
    io $ writeFile "/home/gerson/.cache/xmonad-submap" ""

setSubmap :: String -> X ()
setSubmap name = io $ writeFile "/home/gerson/.cache/xmonad-submap" name

namedSubmap :: String -> Map.Map (KeyMask, KeySym) (X ()) -> X ()
namedSubmap name mp = do
    setSubmap name
    submap mp
    setSubmap ""

openSubmap =
    namedSubmap "Open" $
        Map.fromList
            [ ((0, xK_e), spawn mEditor)
            , ((0, xK_b), spawn mBrowser)
            , ((0, xK_q), spawn mCalc)
            , ((0, xK_f), spawn mFiles)
            , ((0, xK_z), spawn "boomer")
            ]

rofiSubmap =
    namedSubmap "Rofi" $
        Map.fromList
            [ ((0, xK_r), spawn "rofi-run")
            , ((0, xK_a), spawn "rofi-apps")
            , ((0, xK_m), spawn "rofi-music")
            , ((0, xK_s), spawn "rofi-audio")
            , ((0, xK_p), spawn "rofi-capture")
            ]

systemSubmap =
    namedSubmap "System" $
        Map.fromList
            [ ((0, xK_c), spawn "caffeine")
            , ((0, xK_s), spawn "silence")
            , ((0, xK_d), spawn "dunstctl close-all")
            , ((0, xK_x), spawn "xmonad --recompile && xmonad --restart")
            ]

mediaSubmap =
    namedSubmap "Media" $
        Map.fromList
            [ ((0, xK_k), spawn "volume up")
            , ((0, xK_j), spawn "volume down")
            , ((0, xK_m), spawn "volume mute")
            , ((0, xK_v), spawn "volume mute")
            , ((0, xK_p), spawn "playerctl play-pause")
            , ((0, xK_h), spawn "playerctl previous")
            , ((0, xK_l), spawn "playerctl next")
            , ((0, xK_w), spawn "alacritty --class 'wiremix' --command 'wiremix'")
            ]

workspaceSubmap =
    namedSubmap "Workspace" $
        Map.fromList
            [ ((0, xK_t), sendMessage $ JumpToLayout "Tall")
            , ((0, xK_w), sendMessage $ JumpToLayout "Wide")
            , ((0, xK_f), sendMessage $ JumpToLayout "Full")
            , ((0, xK_s), sendMessage $ JumpToLayout "Spiral")
            ]

mKeys =
    [ ("M-<Escape>", spawn "rofi-system")
    , ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')
    , ("M-o", openSubmap)
    , ("M-r", rofiSubmap)
    , ("M-s", systemSubmap)
    , ("M-m", mediaSubmap)
    , ("M-w", workspaceSubmap)
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
    ++
    [ (mask ++ "M-" ++ [key], windows $ action tag)
    | (tag, key) <- zip mWorkspaces "1234567890"
    , (action, mask) <- [(StackSet.greedyView, ""), (StackSet.shift, "S-")]
    ]

toggleFloat w =
    windows
        (\s -> if Map.member w (StackSet.floating s)
               then StackSet.sink w s
               else StackSet.float w (StackSet.RationalRect 0.15 0.15 0.7 0.7) s)

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
