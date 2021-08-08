{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO
import           XMonad

import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops           as Ewmh
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Decoration
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowArranger

import           XMonad.Util.EZConfig                (additionalKeys)
import           XMonad.Util.NamedScratchpad         as NS
import           XMonad.Util.Run                     (spawnPipe)

import           Control.Arrow                       (second, (***))
import qualified Data.Map                            as M
import           Data.Maybe

import qualified XMonad.StackSet                     as W

------------------------------------------------------------------------
-- Utils:
------------------------------------------------------------------------

myTerminal = "alacritty"

------------------------------------------------------------------------
-- Utils:
------------------------------------------------------------------------

centreRect = W.RationalRect 0.5 0.5 0.5 0.5

-- If the window is floating then (f), if tiled then (n)
floatOrNot f n = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats -- if the current window is floating...
    then f
    else n

-- Centre and float a window (retain size)
centreFloat win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
  return ()

-- Float a window in the centre
centreFloat' w = windows $ W.float w centreRect

-- Make a window my 'standard size' (half of the screen) keeping the centre of the window fixed
standardSize win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect x y 0.5 0.5)
  return ()

toggleFloat =
  floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat')

------------------------------------------------------------------------
-- Workspaces:
------------------------------------------------------------------------

myWorkspaces = map show [1 .. 9]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..]

------------------------------------------------------------------------
-- Scratch Pads:
------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm     findTerm                  manageTerm
  , NS "capture"  "org-capture" (title =? "doom-capture") orgFloat
  ]
 where
  spawnTerm  = myTerminal ++ " -t scratchpad"
  findTerm   = title =? "scratchpad"
  orgFloat   = customFloating $ W.RationalRect (1 / 2) (1 / 2) (1 / 2) (1 / 2)
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.6
    w = 0.6
    t = 0.7 - h
    l = 0.8 - w

------------------------------------------------------------------------
-- Key bindings:
------------------------------------------------------------------------

myModMask = mod4Mask
myNumlockMask = mod2Mask

myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $

        -- launch a terminal
       [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)

        -- close focused window
       , ((modMask, xK_w), kill)
       , ((modMask, xK_comma), namedScratchpadAction myScratchPads "terminal")

        -- rofi cmder
       , ((modMask, xK_space), spawn "nimx cmder &")
       , ( (modMask, xK_apostrophe)
         , spawn "rofi-pass -dmenu -theme theme/passmenu.rasi &"
         )
       , ((modMask .|. shiftMask, xK_v), spawn "rofi-greenclip &")

       -- Emacs capture
       -- , ((modMask .|. shiftMask, xK_x), spawn "org-capture-frame &")
       , ( (modMask .|. shiftMask, xK_x)
         , namedScratchpadAction myScratchPads "capture"
         )

       -- Rotate through the available layout algorithms
       , ((modMask .|. shiftMask, xK_m), sendMessage NextLayout)

       --  Reset the layouts on the current workspace to default
       , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ( (modMask .|. controlMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

       -- Move focus to the next window
       , ((modMask, xK_Tab), toggleWS)

       -- Vim window switching
       , ((modMask, xK_h), windowGo L False)
       , ((modMask, xK_l), windowGo R False)
       , ((modMask, xK_j), windowGo D False)
       , ((modMask, xK_k), windowGo U False)
       , ((modMask .|. shiftMask, xK_j), windowSwap D False)
       , ((modMask .|. shiftMask, xK_k), windowSwap U False)
       , ((modMask .|. shiftMask, xK_h), windowSwap L False)
       , ((modMask .|. shiftMask, xK_l), windowSwap R False)
       , ((modMask .|. controlMask, xK_h), sendMessage Shrink)
       , ((modMask .|. controlMask, xK_l), sendMessage Expand)

       -- Move focus to the master window
       , ((modMask, xK_m), windows W.focusMaster)

       -- Swap the focused window and the master window
       , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)

       -- Swap the focused window with the next window
       , ((modMask .|. shiftMask, xK_j), windows W.swapDown)

       -- Swap the focused window with the previous window
       , ((modMask .|. shiftMask, xK_k), windows W.swapUp)

       -- Push window back into tiling
       , ((modMask, xK_t), withFocused $ windows . W.sink)

       -- Deincrement the number of windows in the master area
       , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

       -- Mirror, reflect around x or y axis
       , ((modMask, xK_m), sendMessage $ Toggle MIRROR)
       , ((modMask, xK_v), sendMessage $ Toggle REFLECTY)
       , ((modMask, xK_f), sendMessage $ Toggle FULL)
       , ((modMask .|. shiftMask, xK_f), toggleFloat)

       -- Quit xmonad
       , ( (modMask .|. shiftMask .|. controlMask, xK_q)
         , io (exitWith ExitSuccess)
         )

       -- Reload xmonad
       , ((modMask .|. shiftMask, xK_r), restart "xmonad" True)

       -- windowArranger keybindings
       , ((modMask .|. controlMask, xK_s), sendMessage Arrange)
       , ((modMask .|. controlMask .|. shiftMask, xK_s), sendMessage DeArrange)
       , ((modMask .|. controlMask, xK_Left), sendMessage (MoveLeft 10))
       , ((modMask .|. controlMask, xK_Right), sendMessage (MoveRight 10))
       , ((modMask .|. controlMask, xK_Down), sendMessage (MoveDown 10))
       , ((modMask .|. controlMask, xK_Up), sendMessage (MoveUp 10))
       , ((modMask .|. shiftMask, xK_Left), sendMessage (IncreaseLeft 10))
       , ((modMask .|. shiftMask, xK_Right), sendMessage (IncreaseRight 10))
       , ((modMask .|. shiftMask, xK_Down), sendMessage (IncreaseDown 10))
       , ((modMask .|. shiftMask, xK_Up), sendMessage (IncreaseUp 10))
       , ( (modMask .|. controlMask .|. shiftMask, xK_Left)
         , sendMessage (DecreaseLeft 10)
         )
       , ( (modMask .|. controlMask .|. shiftMask, xK_Right)
         , sendMessage (DecreaseRight 10)
         )
       , ( (modMask .|. controlMask .|. shiftMask, xK_Down)
         , sendMessage (DecreaseDown 10)
         )
       , ( (modMask .|. controlMask .|. shiftMask, xK_Up)
         , sendMessage (DecreaseUp 10)
         )
       ]
    ++
       --
       -- mod-[1..9], Switch to workspace N
       -- mod-shift-[1..9], Move client to workspace N
       --
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

------------------------------------------------------------------------
-- Mouse bindings:
------------------------------------------------------------------------

myMouseBindings (XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $
      -- mod-button1, Set the window to floating mode and move by dragging
      [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

      -- mod-button2, Raise the window to the top of the stack
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

      -- mod-button3, Set the window to floating mode and resize by dragging
      , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
      ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

newtype Flip l a = Flip (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Flip l) a where
  runLayout (W.Workspace i (Flip l) ms) r =
    (map (second flipRect) *** fmap Flip)
      `fmap` runLayout (W.Workspace i l ms) (flipRect r)
   where
    screenWidth = fromIntegral $ rect_width r
    flipRect (Rectangle rx ry rw rh) =
      Rectangle (screenWidth - rx - (fromIntegral rw)) ry rw rh
  handleMessage (Flip l) = fmap (fmap Flip) . handleMessage l
  description (Flip l) = "Flip " ++ description l

myTabConfig = def { activeBorderColor   = "#cd8b00"
                  , activeTextColor     = "#CEFFAC"
                  , activeColor         = "#000000"
                  , inactiveBorderColor = "#7C7C7C"
                  , inactiveTextColor   = "#EEEEEE"
                  , inactiveColor       = "#000000"
                  }
myLayout =
  mkToggle (single MIRROR)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ windowArrange
        (   tiled
        ||| Flip tiled
        ||| Flip Grid
        ||| emptyBSP
        ||| ThreeCol 1 (1 / 3) (3 / 100)
        ||| tabbed shrinkText myTabConfig
        ||| Full
        )
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

------------------------------------------------------------------------
-- Window Rules
------------------------------------------------------------------------

myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  , className =? "mpv" --> doFloat
  , className =? "Pavucontrol" --> doFloat
  ]

------------------------------------------------------------------------
-- Xmobar & Logging
------------------------------------------------------------------------

clickable ws =
  "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myLogHook pipe = dynamicLogWithPP $ xmoPP pipe

xmoPP :: Handle -> PP
xmoPP h = xmobarPP { ppOutput          = hPutStrLn h
                   , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]"
                   , ppVisible         = xmobarColor "#98be65" ""
                   , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" ""
                   , ppHiddenNoWindows = xmobarColor "#c792ea" ""
                   , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60
                   , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"
                   , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
                   , ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                   }

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

defaults pipe = def
  { terminal           = myTerminal
  , focusFollowsMouse  = True
  , borderWidth        = 1
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = "#7c7c7c"
  , focusedBorderColor = "#ffb6b0"

  -- keybindings
  , keys               = myKeys <+> myKeybindings
  , mouseBindings      = myMouseBindings

  -- hooks
  , layoutHook         = avoidStruts $ smartBorders $ myLayout
  , manageHook         = myManageHook
                         <+> manageDocks
                         <+> insertPosition Below Newer
                         <+> namedScratchpadManageHook myScratchPads
  , startupHook        = Ewmh.ewmhDesktopsStartup
  , logHook            = myLogHook pipe
  , handleEventHook    = def
                         <+> Ewmh.ewmhDesktopsEventHook
                         <+> Ewmh.fullscreenEventHook
                         <+> docksEventHook
  }

main = do
  pipe <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ defaults pipe
