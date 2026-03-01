module XMonad.Custom.Bindings
  ( myKeys
  , modMask
  )
where

import           Flow
import           System.Exit
import           XMonad                                   hiding (keys, modMask)
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.EasyMotion                (selectWindow)
import           XMonad.Actions.FloatSnap
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.RepeatAction
import           XMonad.Actions.ShowText
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.WithAll
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Maximize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Man
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad              hiding
                                                          (namedScratchpadFilterOutWorkspace)
import           XMonad.Util.WorkspaceCompare

import qualified Data.Map                                 as M
import qualified XMonad.Layout.Magnifier                  as Mag
import qualified XMonad.StackSet                          as S

import           XMonad.Custom.Actions.ApplicationChooser
import           XMonad.Custom.Actions.DoPrompt
import           XMonad.Custom.Actions.LayoutChooser
import           XMonad.Custom.Actions.Minimize
import           XMonad.Custom.Actions.ScratchpadChooser  (selectScratchpadByName)
import           XMonad.Custom.Actions.TmuxPrompt
import           XMonad.Custom.Hooks.Layout
import           XMonad.Custom.Prompt
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Search

import qualified XMonad.Custom.Misc                       as C

type Keybinding = (String, X ())

type Keybindings = [Keybinding]

modMask :: KeyMask
modMask = mod4Mask

-- | Combines modifiers, key sequences, actions, and a function to create keybindings.
-- Used for creating multiple keybindings with a shared modifier and varying key sequences.
zipKeys ::
  -- | Modifiers
  [a] ->
  -- | Key sequences
  [[a]] ->
  -- | Actions
  [t1] ->
  -- | Binding function
  (t1 -> b) ->
  -- | Resulting keybindings
  [([a], b)]
zipKeys modifiers keySequences actions bindingFunction =
  zipWith (\keys action -> (modifiers ++ keys, bindingFunction action)) keySequences actions

-- | Similar to zipKeys, but allows an additional parameter for the binding function.
-- Useful when the binding function requires an extra argument.
zipKeys' ::
  -- | Modifiers
  [a] ->
  -- | Key sequences
  [[a]] ->
  -- | Actions
  [t1] ->
  -- | Binding function
  (t1 -> t2 -> b) ->
  -- | Extra parameter
  t2 ->
  -- | Resulting keybindings
  [([a], b)]
zipKeys' modifiers keySequences actions bindingFunction param =
  zipWith
    (\keys action -> (modifiers ++ keys, bindingFunction action param))
    keySequences
    actions

tryMessageR_ :: (Message a, Message b) => a -> b -> X ()
tryMessageR_ x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

toggleCopyToAll :: X ()
toggleCopyToAll =
  wsContainingCopies >>= \case
    [] -> windows copyToAll
    _ -> killAllOtherCopies

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS =
  findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= windows <. S.view
prevNonEmptyWS =
  findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= windows <. S.view

toggleFloat :: Window -> X ()
toggleFloat win = windows $ \windowSet ->
  if M.member win (S.floating windowSet)
    then sinkWindow win windowSet
    else floatWindow win defaultRect windowSet
  where
    sinkWindow = S.sink
    floatWindow = S.float
    defaultRect = S.RationalRect (1 / 2 - 1 / 4) (1 / 2 - 1 / 4) (1 / 2) (1 / 2)

integrateOthers :: S.Stack a -> [a]
integrateOthers (S.Stack _ u d) = u <> d

withOthers :: (Window -> X ()) -> X ()
withOthers f = withWindowSet $ mapM_ f <. others
  where
    others =
      maybe [] integrateOthers
        <. S.stack
        <. S.workspace
        <. S.current

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys config = mkKeymap config keys
  where
    keys =
      rememberActions "M-a r" $
        mconcat
          [ keysBase
          , keysMedia
          , keysWindows
          , keysWorkspaces
          , keysSystem
          , keysSpawnables
          , keysResize
          , keysDo
          , keysSearch
          , keysLayout config
          ]

flash' :: String -> X ()
flash' = flashText def 0.5

keysBase :: Keybindings
keysBase =
  [ ("M-r", runOrRaisePrompt promptTheme)
  , ("M-<Space>", shellPrompt $ promptNoCompletion promptTheme)
  , ("M-C-<Space>", spawn "rofilauncher")
  , ("M-S-<Space>", spawn "rofimoji")
  , ("M-M1-<Space>", spawn "rofi-rbw")
  ]

keysMedia :: Keybindings
keysMedia =
  [ ("M-m k", spawn "volume up")
  , ("M-m j", spawn "volume down")
  , ("M-m m", spawn "volume mute")
  , ("M-m v", spawn "volume mute")
  , ("M-m h", spawn "playerctl previous")
  , ("M-m l", spawn "playerctl next")
  , ("M-m p", spawn "playerctl play-pause")
  , ("<XF86AudioRaiseVolume>", spawn "volume up")
  , ("<XF86AudioLowerVolume>", spawn "volume down")
  , ("<XF86AudioMute>", spawn "volume mute")
  , ("M-<XF86AudioMute>", spawn "volume mute")
  ]

keysSystem :: Keybindings
keysSystem =
  [ ("<XF86MonBrightnessDown>", spawn "brightness down")
  , ("<XF86MonBrightnessUp>", spawn "brightness up")
  , ("M-<Escape>", spawn "system")
  , ("M-x p", spawn "capture")
  , ("M-x s", spawn "silence")
  , ("M-x c", spawn "caffeine")
  , ("M-x b", spawn "background alt")
  , ("M-x d", spawn "dunstctl close-all")
  , ("M-x x", confirmPrompt hotPromptTheme "Recompile?" $ spawn "xmonad --recompile && xmonad --restart")
  , ("M-x q", confirmPrompt hotPromptTheme "Quit?" $ io exitSuccess)
  ]

keysSpawnables :: Keybindings
keysSpawnables =
  [ ("M-<Return>", spawn $ C.term C.applications)
  , ("M-S-<Return>", spawn $ C.term C.applications ++ " -e tmux new-session -A -s Home")
  , ("M-o w", spawn $ C.browser C.applications)
  , ("M-o S-w", selectBrowserByNameAndSpawn promptTheme)
  , ("M-o e", spawn $ C.term C.applications ++ " -e nvim")
  , ("M-o S-e", selectEditorByNameAndSpawn promptTheme)
  , ("M-o c", namedScratchpadAction scratchpads "console")
  , ("M-o f", namedScratchpadAction scratchpads "files")
  , ("M-o v", namedScratchpadAction scratchpads "volume")
  , ("M-o m", namedScratchpadAction scratchpads "music")
  , ("M-o t", namedScratchpadAction scratchpads "top")
  , ("M-o b", namedScratchpadAction scratchpads "bluetooth")
  , ("M-o M-o", selectScratchpadByName promptTheme)
  ]

keysDo :: Keybindings
keysDo =
  [ ("M-d s z", spawn $ C.screenZoomer C.applications)
  , ("M-d d", doSomethingPrompt promptTheme)
  , ("M-d w c", workspacePrompt promptTheme $ windows . copy)
  , ("M-d m r", spawn "autorandr --change --force")
  , ("M-d a", doSomethingPrompt promptTheme)
  ]

keysResize :: Keybindings
keysResize =
  [ ("M-[", tryMessageR_ (ExpandTowards L) Shrink)
  , ("M-]", tryMessageR_ (ExpandTowards R) Expand)
  , ("M-S-[", tryMessageR_ (ExpandTowards U) MirrorShrink)
  , ("M-S-]", tryMessageR_ (ExpandTowards D) MirrorExpand)
  , ("M-C-[", tryMessageR_ (ShrinkFrom R) Shrink)
  , ("M-C-]", tryMessageR_ (ShrinkFrom L) Expand)
  , ("M-S-C-[", tryMessageR_ (ShrinkFrom D) MirrorShrink)
  , ("M-S-C-]", tryMessageR_ (ShrinkFrom U) MirrorExpand)
  ]

keysLayout :: XConfig Layout -> Keybindings
keysLayout c =
  [ ("M-d C-S-r", setLayout $ XMonad.layoutHook c)
  , ("M-y", withFocused toggleFloat)
  , ("M-S-y", sinkAll)
  , ("M-S-,", sendMessage $ IncMasterN (-1))
  , ("M-S-.", sendMessage $ IncMasterN 1)
  , ("M-f", sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL])
  , ("M-S-f", withFocused $ sendMessage . maximizeRestore)
  , ("M-t g", toggleGaps)
  , ("M-t s", toggleStatusBar)
  , ("M-t z", toggleZen >> flash' "<~ ZEN ~>")
  ]

keysWindows :: Keybindings
keysWindows =
  [ ("M-w k", kill)
  , ("M-w S-k", confirmPrompt hotPromptTheme "Kill all" killAll)
  , ("M-w C-S-k", confirmPrompt hotPromptTheme "Kill others" $ withOthers killWindow)
  , ("M-w g", windowPrompt promptTheme Goto allWindows)
  , ("M-w /", windowPrompt promptTheme Goto wsWindows)
  , ("M-w b", windowPrompt promptTheme Bring allWindows)
  , ("M-d w S-c", windowPrompt promptTheme BringCopy allWindows)
  , ("M-w c", toggleCopyToAll)
  , ("M-w o", sendMessage Mag.Toggle)
  , ("M-w S-c", kill1)
  , ("M-w d h o", withOthers minimizeWindow)
  , ("M-w h", withFocused minimizeWindow)
  , ("M-w M1-h", withOthers minimizeWindow)
  , ("M-w S-h", withLastMinimized maximizeWindowAndFocus)
  , ("M-w C-S-h", selectMaximizeWindowPrompt $ promptNoCompletion promptTheme)
  , ("M-w r", sendMessage $ Toggle REFLECTX)
  , ("M-w t", withFocused $ sendMessage . MergeAll)
  , ("M-w S-t", withFocused $ sendMessage . UnMerge)
  , ("M-w u", focusUrgent)
  , ("M-w m", windows S.focusMaster)
  , ("M-w S-m", whenX (swapHybrid True) dwmpromote)
  , ("M-w <Space>", selectWindow def >>= (`whenJust` windows . S.focusWindow))
  , ("M-w S-<Space>", selectWindow def >>= (`whenJust` windows . (S.shiftMaster .) . S.focusWindow))
  , ("M-w C-S-<Space>", selectWindow def >>= (`whenJust` killWindow))
  , ("M-w s h", selectWindow def >>= (`whenJust` minimizeWindow))
  , ("M-/", windows S.focusDown)
  , ("M-S-/", windows S.focusUp)
  , ("M-u", onGroup S.focusDown')
  , ("M-i", onGroup S.focusUp')
  , ("M-S-u", windows S.swapDown)
  , ("M-S-i", windows S.swapUp)
  ]
    ++ zipKeys' "M-" vimKeys directions windowGo True
    ++ zipKeys' "M-S-" vimKeys directions windowSwap True
    ++ zipKeys "M-C-" vimKeys directions (sendMessage . pullGroup)
    ++ zipKeys' "M-" arrowKeys directions screenGo True
    ++ zipKeys' "M-S-" arrowKeys directions windowToScreen True
    ++ zipKeys' "M-C-" arrowKeys directions screenSwap True
    ++ zipKeys' "M1-<Space> " vimKeys directions screenSwap True
    ++ zipKeys' "M1-<Space> S-" vimKeys directions windowToScreen True
  where
    directions = [D, U, L, R]
    arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
    vimKeys = ["j", "k", "h", "l"]

keysWorkspaces :: Keybindings
keysWorkspaces =
  [ ("M-p /", switchProjectPrompt promptTheme)
  , ("M-p c", switchProjectPrompt $ promptNoCompletion promptTheme)
  , ("M-p s", gridselectWorkspace gridSelectTheme S.greedyView)
  , ("M-p S-s", shiftToProjectPrompt promptTheme)
  , ("M-p n", renameProjectPrompt hotPromptTheme)
  , ("M-p <Backspace>", removeWorkspace)
  , ("M-p S-<Backspace>", confirmPrompt hotPromptTheme "Kill workspace?" $ killAll >> removeWorkspace)
  , ("M-.", nextNonEmptyWS)
  , ("M-,", prevNonEmptyWS)
  , ("M-;", toggleWS' ["NSP"])
  , ("M-n", workspacePrompt promptTheme $ windows . S.shift)
  , ("M-<Tab>", cycleRecentNonEmptyWS [xK_Alt_L, xK_Alt_R, xK_Escape] xK_comma xK_period)
  ]
    ++ zipKeys "M-" wsKeys [0 ..] (withNthWorkspace S.greedyView)
    ++ zipKeys "M-S-" wsKeys [0 ..] (withNthWorkspace S.shift)
    ++ zipKeys "M-C-S-" wsKeys [0 ..] (withNthWorkspace copy)
  where
    wsKeys = show <$> [1 .. 9 :: Int]

keysSearch :: Keybindings
keysSearch =
  [ ("M-s e", selectAndSearchPrompt $ promptNoCompletion promptTheme)
  , ("M-s m", manPrompt $ simplestSearch promptTheme)
  , ("M-s t", tmuxPrompt promptTheme)
  , ("M-s w", switchProjectPrompt promptTheme)
  , ("M-s s", windowPrompt promptTheme Goto allWindows)
  , ("M-s l", selectLayoutByName promptTheme)
  , ("M-s S-l", selectLayoutGrid)
  , ("M-s c", spawn "xcolor | tr -d '[:space:]' | xclip -selection clipboard")
  ]
