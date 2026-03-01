module XMonad.Custom.Scratchpads
  ( scratchpads
  , namedScratchpadFilterOutWorkspace
  )
where

import           XMonad.Core
import           XMonad.ManageHook
import           XMonad.Util.NamedScratchpad        hiding
                                                    (namedScratchpadFilterOutWorkspace)
import           XMonad.Util.WorkspaceCompare

import           XMonad.Custom.Manage.ManageHelpers
import           XMonad.Custom.Misc                 as C

spawnTerminalWith :: String -> String -> String
spawnTerminalWith className command = unwords $ terminal : options
  where
    terminal = term applications
    options = ["--class", className, "--command", command]

floatingNSP :: ManageHook
floatingNSP = centerFloat w h
  where
    w = 1 / 2
    h = 1 / 1.5

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "console" (spawnTerminalWith "NSPConsole" "tmux new-session -A -s Home") (className =? "NSPConsole") doFullCenterFloat
  , NS "files" (spawnTerminalWith "NSPFiles" "ranger") (className =? "NSPFiles") doFullCenterFloat
  , NS "volume" (spawnTerminalWith "NSPVolume" "wiremix") (className =? "NSPVolume") floatingNSP
  , NS "top" (spawnTerminalWith "NSPTop" "btm") (className =? "NSPTop") doFullCenterFloat
  , NS "notes" (spawnTerminalWith "NSPNotes" "nvim ~/Notes/") (className =? "NSPNotes") doFullCenterFloat
  , NS "logs" (spawnTerminalWith "NSPLogs" "journalctl -f") (className =? "NSPLogs") doFullCenterFloat
  , NS "calculator" (spawnTerminalWith "NSPCalc" "qalc") (className =? "NSPCalc") (centerFloat 0.4 0.4)
  , NS "music" (spawnTerminalWith "NSPMusic" "kew") (className =? "NSPMusic") (centerFloat 0.4 0.4)
  , NS "bluetooth" (spawnTerminalWith "NSPTrans" "bluetui") (className =? "NSPBluetooth") (centerFloat 0.5 0.5)
  ]
  where
    doFullCenterFloat = centerFloat 0.85 0.85

namedScratchpadFilterOutWorkspace :: WorkspaceSort
namedScratchpadFilterOutWorkspace = filterOutWs [scratchpadWorkspaceTag]
