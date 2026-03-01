module XMonad.Custom.Misc
  ( Applications (..)
  , applications
  )
where

data Applications = Applications
  { browser :: !String
  , term    :: !String
  , editor  :: !String
  }
  deriving (Eq, Show)

applications :: Applications
applications =
  Applications
    { browser = "qutebrowser"
    , term = "alacritty"
    , editor = "emacsclient --create-frame --alternate-editor 'emacs'"
    }
