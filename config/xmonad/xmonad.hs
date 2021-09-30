{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           System.Exit
import           System.IO
import           XMonad                              hiding ((|||))
import           XMonad.Core                         (withWindowSet)

import           XMonad.Actions.CopyWindow           (copyToAll,
                                                      killAllOtherCopies,
                                                      wsContainingCopies)

import           XMonad.Layout.LayoutCombinators     (JumpToLayout (JumpToLayout),
                                                      (|||))
import           XMonad.Layout.Named                 (named)

import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize       as Flex
import           XMonad.Actions.FloatKeys            (keysMoveWindow,
                                                      keysMoveWindowTo)
import           XMonad.Actions.GroupNavigation      (Direction (Backward, Forward, History),
                                                      historyHook, nextMatch)
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.TagWindows           (addTag, delTag)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty        (dynamicPropertyChange)
import           XMonad.Hooks.EwmhDesktops           as Ewmh
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doRectFloat)
import           XMonad.Hooks.ServerMode             (serverModeEventHook,
                                                      serverModeEventHookCmd')

import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Decoration
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed                (tabbed)
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowArranger

import           XMonad.Util.EZConfig                (additionalKeys,
                                                      additionalKeysP,
                                                      additionalMouseBindings)
import           XMonad.Util.NamedScratchpad         as NS
import           XMonad.Util.Paste                   (sendKey)
import           XMonad.Util.Run                     (spawnPipe)
import           XMonad.Util.Scratchpad


import           Control.Arrow                       (second, (***))
import           Control.Monad                       (when)
import           Control.Monad.Trans                 (lift)
import           Control.Monad.Trans.Maybe

import           Data.Bool                           (bool)
import           Data.List                           (isPrefixOf, sortOn)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid                         (All (..))

import qualified XMonad.StackSet                     as W

------------------------------------------------------------------------
-- Consts:
------------------------------------------------------------------------

myTerminal = "alacritty"
myMaxScreenCount = 1
------------------------------------------------------------------------
-- Utils:
------------------------------------------------------------------------

startAtomicChrome = do
  spawn "emacsclient -e \"(atomic-chrome-start-server)\""
  sendKey (myAltMask .|. shiftMask) xK_comma

bindAll :: [(Query Bool, X ())] -> X ()
bindAll = mapM_ choose
  where choose (mh, action) = withFocused $ \w -> whenX (runQuery mh w) action

-- | Run the action paired with the first Query that holds true.
bindFirst :: [(Query Bool, X ())] -> X ()
bindFirst = withFocused . chooseOne

chooseOne :: [(Query Bool, X ())] -> Window -> X ()
chooseOne []             _ = return ()
chooseOne ((mh, a) : bs) w = do
  c <- runQuery mh w
  if c then a else chooseOne bs w

xKill :: Window -> X ()
xKill w = withDisplay $ \d -> do
  wmdelt    <- atom_WM_DELETE_WINDOW
  wmprot    <- atom_WM_PROTOCOLS

  protocols <- io $ getWMProtocols d w
  io $ if wmdelt `elem` protocols
    then killClient d w >> return ()
    else killClient d w >> return ()

-- Open terminal in the path that is set in the xproperty "MY_XWINDOW_PATH"
-- otherwise use default terminal
openTerminal = do
  let
    spawnEmptyTerminal = spawn myTerminal

    trySpawnTerminalAtCwd w = do
        cwd <- runQuery (stringProperty "MY_XWINDOW_PATH") w
        case cwd of
          [] -> spawnEmptyTerminal
          xs -> spawn (myTerminal ++ " --working-directory \"" ++ show xs ++ "\"")

    in withWindowSet $ \w -> case (W.peek w) of
        Just win -> trySpawnTerminalAtCwd win
        Nothing  -> spawnEmptyTerminal

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

toggleFloat =
  floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat')

toggleSticky :: X ()
toggleSticky = wsContainingCopies >>= \ws -> case ws of
  [] -> windows copyToAll
  _  -> killAllOtherCopies

-- Put the most recent clicked floating window on top
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w } = do
  s <- gets windowset
  when (w `M.member` W.floating s) $ focus w >> bringFocusedToTop
  pure mempty
floatClickFocusHandler _ = pure mempty

bringFocusedToTop :: X ()
bringFocusedToTop =
  windows $ W.modify' $ \(W.Stack t ls rs) -> W.Stack t [] (reverse ls <> rs)

-- Move window to screen edge
moveWindowToRelativePosition :: Rational -> Rational -> X ()
moveWindowToRelativePosition x y = (() <$) . runMaybeT $ do
  scr <- lift (gets $ W.current . windowset)
  win <- MaybeT (return . fmap W.focus . W.stack . W.workspace $ scr)
  let Rectangle _ _ w h = screenRect . W.screenDetail $ scr
      x0                = round (x * fromIntegral (w - 3))
      y0                = round (y * fromIntegral (h - 27))
  lift (keysMoveWindowTo (x0, y0) (x, y) win)

------------------------------------------------------------------------
-- Workspaces:
------------------------------------------------------------------------

myWorkspaces = map show [1 .. 9]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..]

------------------------------------------------------------------------
-- Scratch Pads:
------------------------------------------------------------------------

namedScratchpads' :: [NamedScratchpad]
namedScratchpads' =
  [NS "terminal" "alacritty -t scratchpad &" (title =? "scratchpad") floating']
  where floating' = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6


-- | Hook for managing scratchpads
scratchpadHook' :: ManageHook
scratchpadHook' = scratchpadManageHook $ W.RationalRect l t w h
 where
  h = 0.25 -- terminal height
  w = 1 -- terminal width
  t = 0
  l = 1 - w

------------------------------------------------------------------------
-- Key bindings:
------------------------------------------------------------------------

myModMask = mod4Mask
myNumlockMask = mod2Mask
myAltMask = mod1Mask

myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $  [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
       , ((modMask, xK_w)     , kill)
       , ( (modMask, xK_comma)
         , namedScratchpadAction namedScratchpads' "terminal"
         )

        -- rofi cmder
       , ((modMask, xK_space), spawn "nimx cmder &")
       , ( (modMask, xK_apostrophe)
         , spawn "rofi-pass -dmenu -theme theme/passmenu.rasi &"
         )
       , ((modMask .|. shiftMask, xK_v)    , spawn "rofi-greenclip &")

       -- Rotate through the available layout algorithms
       , ((modMask .|. shiftMask, xK_m)    , sendMessage NextLayout)

       --  Reset the layouts on the current workspace to default
       , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ( (modMask .|. controlMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

       -- Move focus to the next window
       , ((modMask, xK_Tab)            , toggleWS)

       -- Vim window switching
       , ((modMask, xK_h)              , windowGo L False)
       , ((modMask, xK_l)              , windowGo R False)
       , ((modMask, xK_j)              , windowGo D False)
       , ((modMask, xK_k)              , windowGo U False)
       , ((modMask .|. shiftMask, xK_j), windowSwap D False)
       , ((modMask .|. shiftMask, xK_k), windowSwap U False)
       , ((modMask .|. shiftMask, xK_h), windowSwap L False)
       , ((modMask .|. shiftMask, xK_l), windowSwap R False)
       , ((modMask .|. controlMask, xK_j), do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP" -> sendMessage $ ExpandTowards D
             _     -> sendMessage MirrorShrink
         )
       , ((modMask .|. controlMask, xK_k), do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP" -> sendMessage $ ExpandTowards U
             _     -> sendMessage MirrorExpand
         )
       , ( (modMask .|. controlMask, xK_l)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP"                -> sendMessage $ ExpandTowards R
             "Flip ResizableTall" -> sendMessage $ Shrink
             "Flip ThreeCol"      -> sendMessage $ Shrink
             _                    -> sendMessage Expand
         )
       , ( (modMask .|. controlMask, xK_h)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP"                -> sendMessage $ ExpandTowards L
             "Flip ResizableTall" -> sendMessage $ Expand
             "Flip ThreeCol"      -> sendMessage $ Expand
             _                    -> sendMessage Shrink
         )

       , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)


       -- Move focus to the master window
       , ((modMask, xK_m)                   , windows W.focusMaster)

       -- Swap the focused window and the master window
       , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)

       -- Swap the focused window with the next window
       , ((modMask .|. shiftMask, xK_j)     , windows W.swapDown)

       -- Swap the focused window with the previous window
       , ((modMask .|. shiftMask, xK_k)     , windows W.swapUp)


       -- Deincrement the number of windows in the master area
       , ((modMask, xK_period)              , sendMessage (IncMasterN (-1)))

       -- Mirror, reflect around x or y axis
       , ((modMask, xK_m)                   , sendMessage $ Toggle MIRROR)
       , ((modMask, xK_v)                   , sendMessage $ Toggle REFLECTY)
       , ((modMask, xK_f)                   , sendMessage $ Toggle FULL)
       , ((modMask .|. shiftMask, xK_f)     , toggleFloat)

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


ezKeys :: [(String, X ())]
ezKeys =
  [ ("M-t"       , sendMessage ToggleStruts)
  , ("M-<Return>", openTerminal)
  , ( "M1-S-,"
    , bindFirst
      [ (className =? "Brave-browser", startAtomicChrome)
      , (pure True, sendKey (myAltMask .|. shiftMask) xK_comma)
      ]
    )
  , ("M-S-w x", withFocused xKill)

    -- Move window to corner
  , ( "M-S-w 1"
    , sequence_
      [(moveWindowToRelativePosition 0 0), withFocused (keysMoveWindow (0, 32))]
    )
  , ( "M-S-w 2"
    , sequence_
      [(moveWindowToRelativePosition 1 0), withFocused (keysMoveWindow (0, 32))]
    )
  , ("M-S-w 3", moveWindowToRelativePosition 1 1)
  , ("M-S-w 4", moveWindowToRelativePosition 0 1)
  , ("M-S-w c", moveWindowToRelativePosition 0.5 0.5)

    -- History
  , ("M-S-,"  , nextMatch Backward (return True))
  , ("M-S-."  , nextMatch Forward (return True))
  , ("M-`"    , nextMatch History (return True))

  -- Float / Sink Floating Windows
  , ("M-S-'"  , toggleSticky)
  , ("M-S-t"  , withFocused $ windows . W.sink)

  -- M-o leader
  , ("M-o b"  , spawn "rofi_org_bookmarks")
  ]

------------------------------------------------------------------------
-- External commands

myCommands :: [(String, X ())]
myCommands =
  [ ("decrease-master-size" , sendMessage Shrink)
  , ("increase-master-size" , sendMessage Expand)
  , ("decrease-master-count", sendMessage $ IncMasterN (-1))
  , ("increase-master-count", sendMessage $ IncMasterN (1))
  , ("focus-prev"           , windows W.focusUp)
  , ("focus-next"           , windows W.focusDown)
  , ("toggle-float"         , toggleFloat)
  , ("focus-master"         , windows W.focusMaster)
  , ("swap-with-prev"       , windows W.swapUp)
  , ("swap-with-next"       , windows W.swapDown)
  , ("swap-with-master"     , windows W.swapMaster)
  , ("kill-window"          , kill)
  , ("quit"                 , io $ exitWith ExitSuccess)
  , ("layout-monocle-right"           , sendMessage $ JumpToLayout "Monocle (Right)")
  , ("layout-monocle-left"           , sendMessage $ JumpToLayout "Monocle (Right)")
  , ("layout-monocle-bsp"           , sendMessage $ JumpToLayout "BSP")
  , ("layout-monocle-full"           , sendMessage $ JumpToLayout "Full")
  , ("restart", spawn "xmonad --recompile; xmonad --restart")
  ]

commandsList :: String
commandsList = foldl (\acc cur -> acc ++ (fst cur) ++ "\n") "" myCommands

myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
myCommands' = ("list-commands", listMyServerCmds) : myCommands ++ sortOn
  fst
  (wscs ++ sccs) -- (wscs ++ sccs ++ spcs)
 where
  wscs =
    [ ((m ++ s), windows $ f s)
    | s      <- myWorkspaces
    , (f, m) <- [(W.view, "focus-workspace-"), (W.shift, "send-to-workspace-")]
    ]

  sccs =
    [ ( (m ++ show sc)
      , screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f)
      )
    | sc     <- [0 .. myMaxScreenCount]
    , (f, m) <- [(W.view, "focus-screen-"), (W.shift, "send-to-screen-")]
    ]

        -- spcs = [("toggle-" ++ sp, namedScratchpadAction myScratchpads sp)
        --        | sp <- (flip map) (myScratchpads) (\(NS x _ _ _) -> x) ]

listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "'")
 where
  asmc =
    unlines $ "Available commands:" : map (\(x, _) -> "    " ++ x) myCommands'



------------------------------------------------------------------------
-- Startup Hook:
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  spawn "systemctl --user restart setup-monitor.service"
  spawn "systemctl --user start setup-keyboard.service"

------------------------------------------------------------------------
-- Mouse bindings:
------------------------------------------------------------------------

myMouseBindings (XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $
      -- Set the window to floating mode and move by dragging
      [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

      -- Raise the window to the top of the stack
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

      -- Raise the window to the top of the stack
      , ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
      ]

------------------------------------------------------------------------
-- Layouts
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

getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
  workspaces <- gets windowset
  return $ description . W.layout . W.workspace . W.current $ workspaces

oxyDarkTheme :: Theme
oxyDarkTheme = defaultTheme { inactiveBorderColor = "#777"
                            -- , activeBorderColor = myFocusedBorderColor
                            , activeColor         = "#000"
                            , inactiveColor       = "#444"
                            , inactiveTextColor   = "aquamarine4"
                            , activeTextColor     = "aquamarine1"
                            , fontName            = "xft:Dejavu Sans Mono-8"
                            , decoHeight          = 15
                            , urgentColor         = "#000"
                            , urgentTextColor     = "#63b8ff"
                            }

myLayoutHook =
  -- mkToggle (single MIRROR) $
  mkToggle (NOBORDERS ?? FULL ?? EOT) $ windowArrange
    (   (named "Monocle (Right)" $ tiled)
    ||| (named "Monocle (Left)" $ Flip tiled)
    -- ||| Flip tiled
    -- ||| Flip Grid
    ||| emptyBSP
    -- ||| tabbed shrinkText oxyDarkTheme
    ||| Full
    )
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = ResizableTall nmaster delta ratio []
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

------------------------------------------------------------------------
-- Window Rules
------------------------------------------------------------------------

wmWindowRole = stringProperty "WM_WINDOW_ROLE"

manageWindowsHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , wmWindowRole =? "GtkFileChooserDialog" --> doRectFloat
    (W.RationalRect 0.25 0.25 0.5 0.5)
  , resource =? "kdesktop" --> doIgnore
  , className =? "mpv" --> doFloat
  -- , className =? "Emacs" --> insertPosition Master Newer
  , className =? "Emacs" <&&> title =? "doom-capture" --> doFloat
  , className =? "Pavucontrol" --> doFloat
  ]

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME"
                                          (title =? "Spotify" --> floating)
 where
  floating = customFloating $ W.RationalRect (1 / 12) (1 / 12) (5 / 6) (5 / 6)


------------------------------------------------------------------------
-- Xmobar & Logging
------------------------------------------------------------------------

wrapWorkspaceAction ws =
  "<action=`xdotool set_desktop --relative -- -1` button=4>"
    ++ "<action=`xdotool set_desktop --relative 1` button=5>"
    ++ "<action=xdotool key super+"
    ++ show i
    ++ ">"
    ++ ws
    ++ "</action>"
    ++ "</action>"
    ++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myLogHook pipe = dynamicLogWithPP $ xmoPP pipe

tagHook = withWindowSet $ mapM_ <$> tagFloating <*> W.allWindows
 where
  tagFloating set win = tagIff (win `M.member` W.floating set) "floating" win
  tagIff = bool delTag addTag

xmoPP :: Handle -> PP
xmoPP h = xmobarPP
  { ppOutput          = hPutStrLn h
  , ppCurrent         = xmobarColor "#98be65" "" . wrapWorkspaceAction
  , ppVisible         = xmobarColor "#98be65" "" . wrapWorkspaceAction
  , ppHidden          = wrapWorkspaceAction
  , ppHiddenNoWindows = xmobarColor "#c792ea" "" . wrapWorkspaceAction
  , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60
  , ppSep             = "<fc=#666666> <fn=1>|</fn> </fc>"
  , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort def) -- hide "NSP" from workspace list
  , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
  }

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

defaults pipe =
  def
      { terminal           = myTerminal
      , focusFollowsMouse  = False
      , clickJustFocuses   = False
      , borderWidth        = 1
      , modMask            = myModMask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#7c7c7c"
      , focusedBorderColor = "#ffb6b0"

  -- keybindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

  -- hooks
      , layoutHook         = avoidStruts $ smartBorders $ myLayoutHook
      , manageHook         = manageWindowsHook
                             <+> manageDocks
                             <+> insertPosition Below Newer
                             <+> namedScratchpadManageHook namedScratchpads'
                             <+> scratchpadHook'
      , startupHook        = myStartupHook <+> Ewmh.ewmhDesktopsStartup
      , logHook            = (myLogHook pipe) <+> historyHook <+> tagHook
      , handleEventHook    = def
                             <+> Ewmh.ewmhDesktopsEventHook
                             <+> Ewmh.fullscreenEventHook
                             <+> docksEventHook
                             <+> floatClickFocusHandler
                             <+> myHandleEventHook
                             <+> myServerModeEventHook
      }
    `additionalMouseBindings` []
    `additionalKeysP`         (myNixKeys ++ ezKeys)


main = do
  writeFile "/tmp/xmonad-commands" commandsList
  pipe <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ defaults pipe
