module XMonad.Custom.Config (
    mConfig,
) where

import Flow

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (
    doCenterFloat,
    doRectFloat,
    isDialog,
 )
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral

import XMonad.StackSet qualified as S

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties

import Data.Map qualified as M

import System.IO (writeFile)

import Text.Regex.Posix ((=~))

import XMonad.Custom.Hooks.XMobar qualified as C
import XMonad.Custom.Theme qualified as C

q ~? x = fmap (=~ x) q

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
    mFloat = doRectFloat (S.RationalRect 0.15 0.15 0.7 0.7)

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

namedSubmap :: String -> M.Map (KeyMask, KeySym) (X ()) -> X ()
namedSubmap name mp = do
    setSubmap name
    submap mp
    setSubmap ""

openSubmap =
    namedSubmap "Open" $
        M.fromList
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

systemSubmap =
    namedSubmap "System" $
        M.fromList
            [ ((0, xK_c), spawn "$HOME/.config/xmonad/scripts/caffeine")
            , ((0, xK_s), spawn "$HOME/.config/xmonad/scripts/silence")
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

mKeys =
    [ ("M-<Escape>", spawn "$HOME/.config/rofi/scripts/system")
    , ("M-<Space>", spawn "$HOME/.config/rofi/scripts/launcher")
    , ("M-<Print>", spawn "$HOME/.config/rofi/scripts/capture")
    , ("M-<Return>", spawn mTerminal)
    , ("M-S-<Return>", spawn mTerminal')
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
        , normalBorderColor = C.colorF
        , focusedBorderColor = C.colorN
        , startupHook = myStartupHook
        , layoutHook = mLayoutHook
        , manageHook = mManageHook <+> manageDocks
        }
        `additionalKeysP` mKeys
        |> dynamicSBs C.barSpawner
        |> ewmh
        |> ewmhFullscreen
        |> (return :: a -> IO a)
