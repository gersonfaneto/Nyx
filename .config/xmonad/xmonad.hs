-- import           Flow

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

cBackground = "#1D2021"
cForeground = "#FBF1C7"
cBlack      = "#3C3836"
cBlack'     = "#504945"
cRed        = "#FB4934"
cGreen      = "#B8BB26"
cYellow     = "#FABD2F"
cBlue       = "#83A598"
cMagenta    = "#D3869B"
cCyan       = "#8EC07C"

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

mSystem :: String
mSystem = "alacritty --class btm --command btm"

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

mManageHook =
    composeAll
        [ className ~? "wiremix|btm|ncmpcpp" --> mFloat
        , className =? "Qalculate-gtk" --> mFloat
        , className =? "PrismLauncher" --> doFloat
        , className =? "kdeconnect.app" --> doFloat
        , isDialog --> doFloat
        ]
        <+> insertPosition Below Newer
        where
            mFloat :: ManageHook
            mFloat = doRectFloat (StackSet.RationalRect 0.15 0.15 0.7 0.7)

myStartupHook :: X ()
myStartupHook = do
    spawn "pkill dunst; dunst -config $HOME/.config/dunst/dunstrc &"
    spawn "feh --bg-fill $HOME/.local/share/wallpapers/1920x1080.jpg &"
    spawn "battery -L 20 -n &"
    spawn "xset s 600 &"
    spawn "xss-lock -l -- xsecurelock &"
    spawn "xsetroot -cursor_name left_ptr &"
    spawn "setxkbmap -option ctrl:nocaps -layout br -variant thinkpad &"
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
            , ((0, xK_s), spawn mSystem)
            , ((0, xK_b), spawn mBrowser)
            , ((0, xK_m), spawn "prismlauncher")
            , ((0, xK_w), spawn mSound)
            , ((0, xK_k), spawn "kdeconnect-app")
            , ((0, xK_q), spawn mCalc)
            , ((0, xK_f), spawn mFiles)
            , ((0, xK_z), spawn "boomer")
            ]

rofiSubmap =
    namedSubmap "Rofi" $
        Map.fromList
            [ ((0, xK_r), spawn "$HOME/.config/rofi/scripts/launcher")
            , ((0, xK_p), spawn "$HOME/.config/rofi/scripts/capture")
            ]

systemSubmap =
    namedSubmap "System" $
        Map.fromList
            [ ((0, xK_c), spawn "$HOME/.config/xmonad/scripts/caffeine")
            , ((0, xK_s), spawn "$HOME/.config/xmonad/scripts/silence")
            , ((0, xK_d), spawn "dunstctl close-all")
            , ((0, xK_x), spawn "xmonad --recompile && xmonad --restart")
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
    [ ("M-<Escape>", spawn "$HOME/.config/rofi/scripts/system")
    , ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')
    , ("M-o", openSubmap)
    , ("M-r", rofiSubmap)
    , ("M-s", systemSubmap)
    , ("M-w", workspaceSubmap)
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
    , ("M-z", incWindowSpacing 8)
    , ("M-x", decWindowSpacing 8)
    , ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
    , ("M-S-a", setWindowSpacing (Border 8 8 8 8) >> setScreenSpacing (Border 8 8 8 8))
    , ("<XF86AudioRaiseVolume>", spawn "$HOME/.config/xmonad/scripts/volume up")
    , ("<XF86AudioLowerVolume>", spawn "$HOME/.config/xmonad/scripts/volume down")
    , ("<XF86AudioMute>", spawn "$HOME/.config/xmonad/scripts/volume mute")
    , ("M-<XF86AudioMute>", spawn "$HOME/.config/xmonad/scripts/volume mute")
    , ("M-M1-k", spawn "$HOME/.config/xmonad/scripts/volume up")
    , ("M-M1-j", spawn "$HOME/.config/xmonad/scripts/volume down")
    , ("M-M1-m", spawn "$HOME/.config/xmonad/scripts/volume mute")
    , ("M-M1-v", spawn "$HOME/.config/xmonad/scripts/volume mute")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("M-M1-h", spawn "playerctl previous")
    , ("M-M1-l", spawn "playerctl next")
    , ("M-M1-p", spawn "playerctl play-pause")
    , ("<XF86MonBrightnessDown>", spawn "$HOME/.config/xmonad/scripts/brightness down")
    , ("<XF86MonBrightnessUp>", spawn "$HOME/.config/xmonad/scripts/brightness up")
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

mXmobar = statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (pure myXmobarPP)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB mXmobar defToggleStrutsKey $ myConfig
