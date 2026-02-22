module XMonad.Custom.Prompt
  ( aListCompFunc
  , promptTheme
  , hotPromptTheme
  , promptThemeVim
  , promptNoCompletion
  , promptNoHistory
  , gridSelectTheme
  , helpPromptConfig
  , simplestSearch
  )
where

import           Data.List
import           Data.Ratio
import           XMonad
import           XMonad.Actions.GridSelect
import           XMonad.Actions.ShowText
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch

import qualified XMonad.Custom.Theme       as T

promptNoHistory :: XPConfig -> XPConfig
promptNoHistory ptheme = ptheme {historyFilter = const [], historySize = 0}

promptNoCompletion :: XPConfig -> XPConfig
promptNoCompletion ptheme = promptNoHistory ptheme {autoComplete = Nothing}

simplestSearch :: XPConfig -> XPConfig
simplestSearch ptheme = ptheme {searchPredicate = isPrefixOf, sorter = const id}

promptTheme, hotPromptTheme, promptThemeVim :: XPConfig
promptTheme =
  def
    { font = T.font
    , bgColor = T.black
    , fgColor = T.white
    , fgHLight = T.white'
    , bgHLight = T.black'
    , borderColor = T.white'
    , promptBorderWidth = T.border
    , height = T.height
    , defaultText = ""
    , position = CenteredAt (3 % 10) (2 % 5)
    , maxComplRows = Just 15
    , maxComplColumns = Just 3
    , alwaysHighlight = True
    , historyFilter = const []
    , historySize = 0
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , -- searchPredicate = fuzzyMatch,
      -- sorter = fuzzySort,
      complCaseSensitivity = CaseInSensitive
    , promptKeymap = defaultXPKeymap
    , autoComplete = Just 0
    , completionKey = (0, xK_Down)
    , prevCompletionKey = (0, xK_Up)
    , showCompletionOnTab = False
    }
promptThemeVim = promptTheme {promptKeymap = vimLikeXPKeymap}
hotPromptTheme =
  promptNoCompletion $
    promptTheme
      { bgColor = T.black'
      , fgColor = T.white'
      , fgHLight = T.white
      , bgHLight = T.black
      }

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
  fBC <- asks (focusedBorderColor . config)
  nBC <- asks (normalBorderColor . config)
  pure $ if isFg then (fBC, nBC) else (nBC, fBC)

gridSelectTheme :: GSConfig a
gridSelectTheme = (buildDefaultGSConfig colorizer) {gs_font = T.font}

-- | Optimized completion function for filtering tuples by first element
aListCompFunc :: XPConfig -> [(String, a)] -> String -> IO [String]
aListCompFunc c xs s = pure $! [x | (x, _) <- xs, searchPredicate c s x]

helpPromptConfig :: ShowTextConfig
helpPromptConfig = def {st_font = "xft:monospace:size=12"}
