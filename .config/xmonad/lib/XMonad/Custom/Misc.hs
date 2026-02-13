{-# OPTIONS_GHC -funbox-strict-fields #-}

module XMonad.Custom.Misc
  ( Applications (..)
  , applications
  )
where

data Applications = Applications
  { browser               :: !String
  , mixer                 :: !String
  , notify                :: !String
  , player                :: !String
  , soundEffects          :: !String
  , term                  :: !String
  , term'                 :: !String
  , top                   :: !String
  , reader                :: !String
  , editor                :: !String
  , appmenu               :: !String
  , virtualMachinesManger :: !String
  , screenZoomer          :: !String
  }
  deriving (Eq, Show)

applications :: Applications
applications =
  Applications
    { browser = "alacritty"
    , mixer = "wiremix"
    , notify = "notify-send"
    , player = "spotify"
    , soundEffects = "easyeffects"
    , term = "alacritty"
    , term' = "ghostty"
    , top = "btm"
    , reader = "zathura"
    , editor = "emacsclient --create-frame --alternate-editor 'emacs'"
    , appmenu = "rofilauncher"
    , virtualMachinesManger = "virt-manager"
    , screenZoomer = "boomer"
    }
