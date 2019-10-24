import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
--import XMonad.Layout.Reflect
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.IO

--
-- Constants
--
borderGap = 3

--
-- Keys
--
xF86AudioMute        = (0, 0x1008FF12)
xF86AudioLowerVolume = (0, 0x1008FF11)
xF86AudioRaiseVolume = (0, 0x1008FF13)
xF86AudioPrev        = (0, 0x1008FF16)
xF86AudioNext        = (0, 0x1008FF17)
xF86AudioPlay        = (0, 0x1008FF14)
terminalLaunch       = (mod4Mask, 0x79)

--
-- Layouts
--
myLayout = --smartSpacing borderGap
  (gaps [(U, borderGap),(D, borderGap),(L, borderGap),(R, borderGap)] $
    (smartSpacing borderGap $ (smartBorders (GridRatio (4/3)))) |||
    (smartSpacing borderGap $ (smartBorders (Tall 1 (1/20) (5/9)))) |||
    (smartSpacing borderGap $ (smartBorders (GridRatio (1/2)))) |||
    (smartSpacing borderGap $ (smartBorders (GridRatio (2/2)))) |||
    --(smartSpacing borderGap $ (smartBorders (Tall 1 (1/20) (2/3)))) |||
    (smartSpacing borderGap $ (smartBorders (Tall 1 (1/20) (1/3)))) |||
    (smartSpacing borderGap $ (smartBorders (Tall 1 (1/20) (2/3)))) |||
    (smartSpacing borderGap $ (smartBorders (Mirror (Tall 1 (1/20) (7/10))))) |||
    (smartSpacing borderGap $ (smartBorders (Full))))

main = do
    xmonad $ defaultConfig
      { modMask = mod4Mask
      , layoutHook = myLayout
      , startupHook = setWMName "LG3D"
      , focusedBorderColor = "#d33682"
      , normalBorderColor  = "#073642"
      , manageHook = manageDocks
      }
      `additionalKeys`
      [ (xF86AudioLowerVolume         , spawn "amixer set Master 5-")
      , (xF86AudioRaiseVolume         , spawn "amixer set Master 5+")
      , (xF86AudioMute                , spawn "amixer -D pulse set Master toggle")
      , (terminalLaunch               , spawn "terminator")
      ]
