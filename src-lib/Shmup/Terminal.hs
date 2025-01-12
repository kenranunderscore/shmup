{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Shmup.Terminal where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace
import Foreign (Ptr)
import Foreign.C
import Foreign.Marshal.Safe qualified as Marshal
import Foreign.Ptr (nullPtr)
import Raylib.Core
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Shmup.Terminal.Parser
import System.Posix qualified as Posix
import System.Posix.ByteString qualified as PosixBS

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= (`when` action)

readPty :: Posix.Fd -> IO (TVar BS.ByteString)
readPty fd = do
    res <- newTVarIO mempty
    _ <- forkIO $ forever $ do
        output <- PosixBS.fdRead fd 4096
        traceM $ "  OUTPUT: " <> show output
        atomically $ modifyTVar' res (<> output)
    pure res

data Modifier
    = Ctrl
    | Alt
    | Shift
    deriving stock (Show, Eq, Ord)

keyModPairs :: [((KeyboardKey, KeyboardKey), Modifier)]
keyModPairs =
    [ ((KeyLeftShift, KeyRightShift), Shift)
    , ((KeyLeftAlt, KeyRightAlt), Alt)
    , ((KeyLeftControl, KeyRightControl), Ctrl)
    ]

updateModifiers :: Set Modifier -> IO (Set Modifier)
updateModifiers mods = do
    foldM
        ( \acc ((left, right), m) -> do
            leftDown <- isKeyDown left
            rightDown <- isKeyDown right
            if leftDown || rightDown
                then pure $ Set.insert m acc
                else pure $ Set.delete m acc
        )
        mods
        keyModPairs

readKeys :: IO String
readKeys = reverse <$> go []
  where
    go acc = do
        key <- getKeyPressed
        when (key /= KeyNull) (traceM $ "   key: " <> show key)
        let i = fromEnum key
        let c = Char.toLower (Char.chr i)
        if
            | key == KeySpace -> go (c : acc)
            | key == KeyEnter -> go ('\r' : acc)
            | i > 96 -> go acc
            | i < 39 -> pure acc
            | otherwise -> go (c : acc)

fontSize :: Float
fontSize = 40

mainLoop ::
    WindowResources ->
    Font ->
    Set Modifier ->
    Vector2 ->
    TVar BS.ByteString ->
    Posix.Fd ->
    IO ()
mainLoop window font mods dimensions@(Vector2 w h) content fd = do
    whenM (not <$> windowShouldClose) $ do
        input <- readKeys
        newMods <- updateModifiers mods
        Posix.fdWrite fd input
        output <- readTVarIO content
        let termOutput = parseOutput output
        drawing $ do
            clearBackground black
            drawFPS 1800 0
            Marshal.with font $ \font' -> do
                Marshal.with green $ \green' -> do
                    foldM_
                        ( \(row, col) -> \case
                            Glyph c -> do
                                case c of
                                    '\r' -> pure (row, col)
                                    '\n' -> pure (row + 1, 0)
                                    _ -> do
                                        let x = realToFrac col * w
                                        let y = realToFrac row * h
                                        drawTextEx' font' [c] x y fontSize 0 green'
                                        pure (row, col + 1)
                            _ -> pure (row, col)
                        )
                        (0 :: Integer, 0 :: Integer)
                        termOutput
        mainLoop window font newMods dimensions content fd

foreign import ccall "ioctl" ioctl :: Posix.Fd -> CUInt -> Ptr () -> IO CInt
foreign import ccall "setsid" setsid :: IO ()
foreign import ccall unsafe "draw_text_ex_no_vec" drawTextExNoVec :: Ptr Font -> CString -> Float -> Float -> Float -> Float -> Ptr Color -> IO ()

{-# INLINE drawTextEx' #-}
drawTextEx' :: Ptr Font -> String -> Float -> Float -> Float -> Float -> Ptr Color -> IO ()
drawTextEx' font text x y size spacing color =
    withCString text $ \s ->
        drawTextExNoVec font s x y size spacing color

tiocsctty :: CUInt
tiocsctty = 21518

run :: IO ()
run = do
    (primary, secondary) <- Posix.openPseudoTerminal
    Posix.forkProcess $ do
        Posix.closeFd primary
        -- TODO: understand
        -- TODO: error handling
        setsid
        void $ ioctl secondary tiocsctty nullPtr
        void $ Posix.dupTo secondary Posix.stdInput
        void $ Posix.dupTo secondary Posix.stdError
        void $ Posix.dupTo secondary Posix.stdOutput
        Posix.closeFd secondary
        Posix.executeFile "bash" True [] Nothing
    Posix.closeFd secondary
    content <- readPty primary
    withWindow 2000 1500 "the best is yet to shmup" 2000 $ \window -> do
        font <-
            managed window $
                loadFontEx
                    "/home/void/.nix-profile/share/fonts/truetype/JetBrainsMono-Regular.ttf"
                    (round fontSize)
                    mempty
        dimensions <- measureTextEx font "m" fontSize 0
        mainLoop window font mempty dimensions content primary
