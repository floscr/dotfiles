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
import           XMonad.Layout.PerWorkspace          (onWorkspace)

import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize       as Flex
import           XMonad.Actions.FloatKeys            (keysMoveWindow,
                                                      keysMoveWindowTo)
import           XMonad.Actions.GroupNavigation      (Direction (Backward, Forward, History),
                                                      historyHook, nextMatch)
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.TagWindows           (addTag, delTag, hasTag)


import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.DynamicProperty        (dynamicPropertyChange)
import           XMonad.Hooks.EwmhDesktops           as Ewmh
import           XMonad.Hooks.PositionStoreHooks

import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, doRectFloat,
                                                      isFullscreen,
                                                      isInProperty)
import           XMonad.Hooks.Place                  (inBounds, placeHook,
                                                      underMouse)
import           XMonad.Hooks.RefocusLast            (isFloat,
                                                      refocusLastLayoutHook,
                                                      refocusLastWhen)
import           XMonad.Hooks.ServerMode             (serverModeEventHook,
                                                      serverModeEventHookCmd')


import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Decoration
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiColumns          (multiCol)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, MIRROR, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed                (tabbed)
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.WindowArranger

import           XMonad.Util.EZConfig                (additionalKeys,
                                                      additionalKeysP,
                                                      additionalMouseBindings)
import           XMonad.Util.NamedScratchpad         as NS
import           XMonad.Util.Paste                   (sendKey)
import           XMonad.Util.PositionStore
import           XMonad.Util.Run                     (spawnPipe)
import           XMonad.Util.Scratchpad

import           Control.Arrow                       (second, (***))
import           Control.Monad                       (filterM, when)
import           Control.Monad.Trans                 (lift)
import           Control.Monad.Trans.Maybe

import           Data.Bool                           (bool)
import           Data.List                           (isPrefixOf, sortOn)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid                         (All (..), appEndo)
import qualified Data.Text                           as T


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
bindAll = mapM_ choose where choose (mh, action) = withFocused $ \w -> whenX (runQuery mh w) action

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
  io $ if wmdelt `elem` protocols then killClient d w >> return () else killClient d w >> return ()

willFloat :: Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
  sh <- io $ getWMNormalHints d w
  let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
  isTransient <- isJust <$> io (getTransientForHint d w)
  return (isFixedSize || isTransient)

-- If the window is floating then (f), if tiled then (n)
floatOrNot f n = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats -- if the current window is floating...
    then f
    else n

doCenterFloatRetainSize win = do
  (_, W.RationalRect x y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - (min h 0.9)) / 2) w (min h 0.9))
  return ()

toggleFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused $ doCenterFloatRetainSize)

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
bringFocusedToTop = windows $ W.modify' $ \(W.Stack t ls rs) -> W.Stack t [] (reverse ls <> rs)

-- Move window to screen edge
moveWindowToRelativePosition :: Rational -> Rational -> X ()
moveWindowToRelativePosition x y = (() <$) . runMaybeT $ do
  scr <- lift (gets $ W.current . windowset)
  win <- MaybeT (return . fmap W.focus . W.stack . W.workspace $ scr)
  let Rectangle _ _ w h = screenRect . W.screenDetail $ scr
      x0                = round (x * fromIntegral (w - 3))
      y0                = round (y * fromIntegral (h - 27))
  lift (keysMoveWindowTo (x0, y0) (x, y) win)


-- https://github.com/SimSaladin/configs/blob/646a363ed2f47db190e41a4ed58808687f92e0dd/.xmonad/xmonad.hs
-- | Float current according to saved position
myFloatCurrent :: X ()
myFloatCurrent = withFocused $ \window -> withWindowSet $ \ws -> do
  ps <- getPosStore
  let sr@(Rectangle _srX _srY srW srH) = screenRect . W.screenDetail $ W.current ws
  case posStoreQuery ps window sr of
    Just (Rectangle x y w h) -> do
      let r' = W.RationalRect (fromIntegral x / fromIntegral srW)
                              (fromIntegral y / fromIntegral srH)
                              (fromIntegral w / fromIntegral srW)
                              (fromIntegral h / fromIntegral srH)
      windows $ W.float window r'
    Nothing -> return ()
-- | Save float position of the window
saveFloatPosition :: Window -> X ()
saveFloatPosition window = do
  sr        <- withWindowSet $ return . screenRect . W.screenDetail . W.current
  (_, rect) <- floatLocation window
  modifyPosStore $ \ps -> posStoreInsert ps window (scaleRationalRect sr rect) sr

------------------------------------------------------------------------
-- Workspaces:
------------------------------------------------------------------------

myWorkspaces = map show [1 .. 9]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..]

------------------------------------------------------------------------
-- Scratch Pads:
------------------------------------------------------------------------

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminal"      "alacritty -t scratchpad &"        (title =? "scratchpad")    floating'
  , NS "emacs-scratch" "emacsclient -e \"(+UI|scratch)\"" (title =? "emacs-scratch") floating'
  ]
  where floating' = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6

-- | Hook for managing scratchpads
myScratchpadHook :: ManageHook
myScratchpadHook = scratchpadManageHook $ W.RationalRect l t w h
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
    $  [ ((modMask, xK_Return)               , spawn $ XMonad.terminal conf)
       , ((modMask, xK_w)                    , kill)
       , ((modMask, xK_comma), namedScratchpadAction myScratchpads "terminal")


       -- Rotate through the available layout algorithms
       , ((modMask .|. shiftMask, xK_m)      , sendMessage NextLayout)

       --  Reset the layouts on the current workspace to default
       , ((modMask .|. shiftMask, xK_space)  , setLayout $ XMonad.layoutHook conf)
       , ((modMask .|. controlMask, xK_space), setLayout $ XMonad.layoutHook conf)

       -- Move focus to the next window
       , ((modMask, xK_Tab)                  , toggleWS)

       -- Vim window switching
       , ((modMask, xK_h)                    , windowGo L False)
       , ((modMask, xK_l)                    , windowGo R False)
       , ((modMask, xK_j)                    , windowGo D False)
       , ((modMask, xK_k)                    , windowGo U False)
       , ((modMask .|. shiftMask, xK_j)      , windowSwap D False)
       , ((modMask .|. shiftMask, xK_k)      , windowSwap U False)
       , ((modMask .|. shiftMask, xK_h)      , windowSwap L False)
       , ((modMask .|. shiftMask, xK_l)      , windowSwap R False)
       , ( (modMask .|. controlMask, xK_j)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP" -> sendMessage $ ExpandTowards D
             _     -> sendMessage MirrorShrink
         )
       , ( (modMask .|. controlMask, xK_k)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP" -> sendMessage $ ExpandTowards U
             _     -> sendMessage MirrorExpand
         )
       , ( (modMask .|. controlMask, xK_l)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP"            -> sendMessage $ ExpandTowards R
             "Monocle (Left)" -> sendMessage $ Shrink
             _                -> sendMessage Expand
         )
       , ( (modMask .|. controlMask, xK_h)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "BSP"            -> sendMessage $ ExpandTowards L
             "Monocle (Left)" -> sendMessage $ Expand
             _                -> sendMessage Shrink
         )
       , ((modMask .|. shiftMask, xK_space)            , setLayout $ XMonad.layoutHook conf)


       -- Move focus to the master window
       , ((modMask, xK_m)                              , windows W.focusMaster)

       -- Swap the focused window and the master window
       , ((modMask .|. shiftMask, xK_Return)           , windows W.swapMaster)

       -- Swap the focused window with the next window
       , ((modMask .|. shiftMask, xK_j)                , windows W.swapDown)

       -- Swap the focused window with the previous window
       , ((modMask .|. shiftMask, xK_k)                , windows W.swapUp)


       -- Deincrement the number of windows in the master area
       , ((modMask, xK_period)                         , sendMessage (IncMasterN (-1)))

       -- Mirror, reflect around x or y axis
       , ((modMask, xK_m)                              , sendMessage $ Toggle MIRROR)
       , ((modMask, xK_v)                              , sendMessage $ Toggle REFLECTY)
       , ((modMask, xK_f)                              , sendMessage $ Toggle FULL)

       -- Quit xmonad
       , ((modMask .|. shiftMask .|. controlMask, xK_q), io (exitWith ExitSuccess))

       -- Reload xmonad
       , ((modMask .|. shiftMask, xK_r)                , restart "xmonad" True)

       -- windowArranger keybindings
       , ((modMask .|. controlMask, xK_s)              , sendMessage Arrange)
       , ((modMask .|. controlMask .|. shiftMask, xK_s), sendMessage DeArrange)
       , ((modMask .|. controlMask, xK_Left)           , sendMessage (MoveLeft 10))
       , ((modMask .|. controlMask, xK_Right)          , sendMessage (MoveRight 10))
       , ((modMask .|. controlMask, xK_Down)           , sendMessage (MoveDown 10))
       , ((modMask .|. controlMask, xK_Up)             , sendMessage (MoveUp 10))
       , ((modMask .|. shiftMask, xK_Left)             , sendMessage (IncreaseLeft 10))
       , ((modMask .|. shiftMask, xK_Right)            , sendMessage (IncreaseRight 10))
       , ((modMask .|. shiftMask, xK_Down)             , sendMessage (IncreaseDown 10))
       , ((modMask .|. shiftMask, xK_Up)               , sendMessage (IncreaseUp 10))
       , ((modMask .|. controlMask .|. shiftMask, xK_Left), sendMessage (DecreaseLeft 10))
       , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 10))
       , ((modMask .|. controlMask .|. shiftMask, xK_Down), sendMessage (DecreaseDown 10))
       , ((modMask .|. controlMask .|. shiftMask, xK_Up), sendMessage (DecreaseUp 10))
       ]
    ++
       --
       -- mod-[1..9], Switch to workspace N
       -- mod-shift-[1..9], Move client to workspace N
       --
       [ ((m .|. modMask, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]


myKeyboardBindings :: [(String, X ())]
myKeyboardBindings =
  [ ("M-t"         , sendMessage ToggleStruts)
  , ("M-<Return>", spawn (myTerminal ++ " --working-directory \"`xcwd`\""))
  , ("M-S-c", bindFirst [(className =? "Brave-browser", sendKey (controlMask .|. shiftMask) xK_l), (pure True, spawn "org-capture-frame")])
  , ("M1-S-,", bindFirst [(className =? "Brave-browser", startAtomicChrome), (pure True, sendKey (myAltMask .|. shiftMask) xK_comma)])
  , ("M-S-w x"     , withFocused xKill)
  , ("M-S-f"       , toggleFloat)
  , ("M-f"         , toggleFull)
  , ("M-<Space>"   , spawn "rofi_cmder")
  , ("M-'", spawn "rofi-pass -dmenu -theme theme/passmenu.rasi")
  , ("M-S-v"       , spawn "rofi-greenclip")
  , ("M-S-<Return>", namedScratchpadAction myScratchpads "emacs-scratch")
  , ("M-C-'"       , spawn "rofi_org_bookmarks")

    -- Move window to corner
  , ("M-S-w 1", sequence_ [(moveWindowToRelativePosition 0 0), withFocused (keysMoveWindow (0, 32))])
  , ("M-S-w 2", sequence_ [(moveWindowToRelativePosition 1 0), withFocused (keysMoveWindow (0, 32))])
  , ("M-S-w 3"     , moveWindowToRelativePosition 1 1)
  , ("M-S-w 4"     , moveWindowToRelativePosition 0 1)
  , ("M-S-w c"     , moveWindowToRelativePosition 0.5 0.5)

    -- History
  , ("M-S-,"       , nextMatch Backward (return True))
  , ("M-S-."       , nextMatch Forward (return True))
  , ("M-`"         , nextMatch History (return True))

  -- Float / Sink Floating Windows
  , ("M-S-'"       , toggleSticky)
  , ("M-S-t"       , withFocused $ windows . W.sink)

  -- M-o leader
  , ("M-o b"       , spawn "rofi_org_bookmarks")
  ]

------------------------------------------------------------------------
-- External commands

myCommands :: [(String, X ())]
myCommands =
  [ ("decrease-master-size"        , sendMessage Shrink)
  , ("increase-master-size"        , sendMessage Expand)
  , ("decrease-master-count"       , sendMessage $ IncMasterN (-1))
  , ("increase-master-count"       , sendMessage $ IncMasterN (1))
  , ("focus-prev"                  , windows W.focusUp)
  , ("focus-next"                  , windows W.focusDown)
  , ("toggle-float"                , toggleFloat)
  , ("focus-master"                , windows W.focusMaster)
  , ("swap-with-prev"              , windows W.swapUp)
  , ("swap-with-next"              , windows W.swapDown)
  , ("swap-with-master"            , windows W.swapMaster)
  , ("center-float-window", withFocused doCenterFloatRetainSize)
  , ("kill-window"                 , kill)
  , ("quit"                        , io $ exitWith ExitSuccess)
  , ("layout-monocle-right", sendMessage $ JumpToLayout "Monocle (Right)")
  , ("layout-monocle-left", sendMessage $ JumpToLayout "Monocle (Left)")
  , ("layout-monocle-bsp"          , sendMessage $ JumpToLayout "BSP")
  , ("layout-monocle-three-columns", sendMessage $ JumpToLayout "Three Columns")
  , ("layout-monocle-full"         , sendMessage $ JumpToLayout "Full")
  , ("restart", spawn "xmonad --recompile; xmonad --restart")
  ]

commandsList :: String
commandsList = foldl (\acc cur -> acc ++ (fst cur) ++ "\n") "" myCommands

myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
myCommands' = ("list-commands", listMyServerCmds) : myCommands ++ sortOn fst (wscs ++ sccs) -- (wscs ++ sccs ++ spcs)
 where
  wscs = [ ((m ++ s), windows $ f s) | s <- myWorkspaces, (f, m) <- [(W.view, "focus-workspace-"), (W.shift, "send-to-workspace-")] ]

  sccs =
    [ ((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
    | sc     <- [0 .. myMaxScreenCount]
    , (f, m) <- [(W.view, "focus-screen-"), (W.shift, "send-to-screen-")]
    ]

        -- spcs = [("toggle-" ++ sp, namedScratchpadAction myScratchpads sp)
        --        | sp <- (flip map) (myScratchpads) (\(NS x _ _ _) -> x) ]

listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "'") where asmc = unlines $ "Available commands:" : map (\(x, _) -> "    " ++ x) myCommands'



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
      [ ((modMask, button1)  , (\w -> focus w >> mouseMoveWindow w))

      -- Raise the window to the top of the stack
      , ((modMask, button2)  , (\w -> focus w >> windows W.swapMaster))

      -- Raise the window to the top of the stack
      , ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
      ]

------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------

newtype Flip l a = Flip (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Flip l) a where
  runLayout (W.Workspace i (Flip l) ms) r = (map (second flipRect) *** fmap Flip) `fmap` runLayout (W.Workspace i l ms) (flipRect r)
   where
    screenWidth = fromIntegral $ rect_width r
    flipRect (Rectangle rx ry rw rh) = Rectangle (screenWidth - rx - (fromIntegral rw)) ry rw rh
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
  smartBorders
    $ refocusLastLayoutHook
    . trackFloating
    $ avoidStruts
    $ windowArrange
    $ mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ onWorkspace "2" workingLayout
    $ onWorkspace "3" termLayout defaultLayout
 where
  defaultLayout  = monocleRight ||| monocleLeft ||| emptyBSP ||| threeColLayout ||| Full
  workingLayout  = monocleLeft ||| monocleRight ||| emptyBSP ||| threeColLayout ||| Full
  termLayout     = emptyBSP ||| threeColLayout ||| monocleLeft ||| monocleRight ||| Full
  threeColLayout = named "Columns" $ (multiCol [1] 8 0.05 0.33)
  monocleRight   = named "Monocle (Right)" $ tiled
  monocleLeft    = named "Monocle (Left)" $ Flip tiled
  tiled          = ResizableTall 1 0.03 0.8 []

------------------------------------------------------------------------
-- Window Rules
------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
  [ manageWindowsHook
    -- open windows at the end if they are not floating
  , fmap not willFloat --> insertPosition Below Newer
  , manageDocks
  , namedScratchpadManageHook myScratchpads
  , myScratchpadHook
  ]

isOverlayWindow :: Query Bool
isOverlayWindow = do
  isNotification      <- isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
  floatingVideoWindow <- className =? "zoom" <&&> title =? "zoom_linux_float_video_window"
  sharingToolbar      <- className =? "zoom" <&&> title =? "as_toolbar"
  sharingWindowFrame  <- title =? "cpt_frame_window"
  return $ isNotification || floatingVideoWindow || sharingToolbar || sharingWindowFrame

allWindowsByType :: Query Bool -> X [Window]
allWindowsByType query = withDisplay $ \display -> do
  (_, _, windows) <- asks theRoot >>= io . queryTree display
  filterM (runQuery query) windows

raiseWindow' :: Window -> X ()
raiseWindow' window = withDisplay $ \display -> io $ raiseWindow display window

raiseOverlays :: X ()
raiseOverlays = allWindowsByType isOverlayWindow >>= mapM_ raiseWindow'

fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

togglFullFloat w = do
  hasFloatTag <- hasTag "fullFloat" w
  if hasFloatTag
    then do
      myFloatCurrent
      withFocused $ delTag "fullFloat"
    else do
      withFocused $ saveFloatPosition
      fullFloatFocused
      withFocused $ addTag "fullFloat"

toggleFull = withFocused
  (\w -> do
    floats <- gets (W.floating . windowset)
    if w `M.member` floats
      then do
        togglFullFloat w
      else do
        sendMessage $ Toggle FULL
  )

manageWindowsHook = composeAll
  [ isFullscreen --> doFullFloat
  , resource =? "desktop_window" --> doIgnore
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
  , resource =? "kdesktop" --> doIgnore
  , className =? "mpv" --> doFloat
  , className =? "zoom" --> doFloat

  -- Gimp
  , ("Gimp" `isPrefixOf`) <$> className <&&> title =? "gimp-action-search-dialog" --> floating
  , ("Gimp" `isPrefixOf`) <$> className --> doFloatToMouseCenter
  , role =? "gimp-image-window" --> doShift "5"

  , ("steam_app_" `isPrefixOf`) <$> className --> floating
  -- , className =? "Emacs" --> insertPosition Master Newer
  , className =? "Emacs" <&&> title =? "doom-capture" --> doFloat
  , className =? "Pavucontrol" --> doFloatToMouseCenter
  , className =? "Dragon" --> doFloatToMouse (0.05, 0.05)
  ]
 where
  role = stringProperty "WM_WINDOW_ROLE"
  doFloatToMouse       = \pos -> placeHook (inBounds (underMouse pos)) <+> doFloat
  doFloatToMouseCenter = doFloatToMouse (0.5, 0.5)
  floating             = customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4)

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME"
  $ composeAll [title =? "Spotify" --> doCenterFloat, title =? "doom-capture" --> floating, title =? "emacs-scratch" --> floating]
  where floating = customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4)

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
xmoPP h = xmobarPP { ppOutput          = hPutStrLn h
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
      , logHook            = (myLogHook pipe) <+> historyHook <+> tagHook <+> raiseOverlays
      , layoutHook         = myLayoutHook
      , manageHook         = myManageHook
      , startupHook        = myStartupHook <+> Ewmh.ewmhDesktopsStartup
      , handleEventHook    = def
                             <+> refocusLastWhen isFloat
                             <+> Ewmh.ewmhDesktopsEventHook
                             <+> Ewmh.fullscreenEventHook
                             <+> docksEventHook
                             <+> floatClickFocusHandler
                             <+> myHandleEventHook
                             <+> myServerModeEventHook
                             <+> positionStoreEventHook
      }
    `additionalMouseBindings` []
    `additionalKeysP`         (myNixKeys ++ myKeyboardBindings)


main = do
  writeFile "/tmp/xmonad-commands" commandsList
  pipe <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ defaults pipe
