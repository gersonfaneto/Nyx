module XMonad.Custom.Theme where

import           Data.Char
import           Data.Function
import           Data.List
import           Graphics.X11.Xlib.Types
import           XMonad.Layout.Decoration

font :: String
font = "xft:monospace:style=Regular:size=10:antialias=true"

black, black' :: String
(black, black') = ("#0b0806", "#2f2b2a")

red, red' :: String
(red, red') = ("#844d2c", "#a64848")

green, green' :: String
(green, green') = ("#57553a", "#897f5a")

yellow, yellow' :: String
(yellow, yellow') = ("#a17c38", "#c8b38d")

blue, blue' :: String
(blue, blue') = ("#41434f", "#526274")

magenta, magenta' :: String
(magenta, magenta') = ("#6b4444", "#755c47")

cyan, cyan' :: String
(cyan, cyan') = ("#59664c", "#718062")

white, white' :: String
(white, white') = ("#a19782", "#c1ab83")

colorN, colorF :: String
colorN = black'
colorF = white'

gapBase, gapFull :: Int
gapBase = 6
gapFull = gapBase * 2

height, border :: Dimension
height = 16 * 2
border = 2

tabTheme :: Theme
tabTheme =
  def
    { activeColor = black
    , inactiveColor = black'
    , urgentColor = red
    , activeBorderColor = white
    , inactiveBorderColor = white'
    , urgentBorderColor = red'
    , activeTextColor = white
    , inactiveTextColor = white'
    , urgentTextColor = red'
    , fontName = font
    , decoHeight = height
    }
