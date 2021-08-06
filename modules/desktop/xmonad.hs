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

import           XMonad.Layout.Decoration
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowArranger

import           XMonad.Util.EZConfig                (additionalKeys)
import           XMonad.Util.NamedScratchpad         as NS
import           XMonad.Util.Run                     (spawnPipe)


import qualified Data.Map                            as M

import qualified XMonad.StackSet                     as W

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "alacritty"

centreRect = W.RationalRect 0.25 0.25 0.5 0.5

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


myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
--myWorkspaces    = ["1:code","2:web","3:msg","4:vm","5:media","6","7","8","9"]
myWorkspaces = map show [1 .. 9]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws =
  "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices



-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

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
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $

        -- launch a terminal
       [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)

        -- close focused window
       , ((modMask, xK_w), kill)

     -- Rotate through the available layout algorithms
       , ((modMask, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
       , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

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

       -- Increment the number of windows in the master area
       -- , ((modMask, xK_comma), sendMessage (IncMasterN 1))

       -- Deincrement the number of windows in the master area
       , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

       -- Mirror, reflect around x or y axis
       , ((modMask, xK_m), sendMessage $ Toggle MIRROR)
       , ((modMask, xK_v), sendMessage $ Toggle REFLECTY)
       -- , ((modMask .|. shiftMask, xK_v), sendMessage $ Toggle REFLECTY)
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
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $

    -- mod-button1, Set the window to floating mode and move by dragging
      [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
      , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
      ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

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
        ||| ThreeCol 1 (1 / 3) (3 / 100)
        ||| tabbed shrinkText myTabConfig
        ||| Full
        ||| spiral (6 / 7)
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
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  , className =? "mpv" --> doFloat
  ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True



------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

myLogHook h = dynamicLogWithPP $ xmoPP h

xmoPP :: Handle -> PP
xmoPP h = xmobarPP { ppOutput          = hPutStrLn h
                   , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]"
                   , ppVisible         = xmobarColor "#98be65" ""
                   , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" ""
                   , ppHiddenNoWindows = xmobarColor "#c792ea" ""
                   , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60
                   , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"
                   , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
           -- , ppExtras  = [windowCount]
                   , ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                   }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = Ewmh.ewmhDesktopsStartup

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults pipe = def
  {
   -- simple stuff
    terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
--        numlockMask        = myNumlockMask,
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor

      -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings

      -- hooks, layouts
  , layoutHook         = avoidStruts $ smartBorders $ myLayout
  , manageHook         = myManageHook
                         <+> manageDocks
                         <+> insertPosition Below Newer
                         <+> namedScratchpadManageHook myScratchPads
  , startupHook        = myStartupHook
  , logHook            = myLogHook pipe
  , handleEventHook    = def
                         <+> Ewmh.ewmhDesktopsEventHook
                         <+> Ewmh.fullscreenEventHook
                         <+> docksEventHook
  }

main = do
  pipe <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ defaults pipe


-- main = do
--   xmproc <- spawnPipe "xmobar"
--   xmonad defaults
--     { startupHook = setWMName "LG3D"
--     }
