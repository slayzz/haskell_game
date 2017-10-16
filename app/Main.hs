{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.Suspend.Lifted
import           Control.Concurrent.Timer
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           System.Console.ANSI
import           System.Console.Terminal.Size
import           System.IO
import           System.Random
import           GHC.Event

class EqualsStringColors a where
    eq :: a -> Bool

data ColorPrinter = ColorPrinter { getColor       :: Color
                                 , getColorString :: String }
                    deriving (Show)

data Player = Player1 Int | Player2 Int
            deriving (Eq, Show)

data GameState = GameState (Player, Player)
               deriving (Eq, Show)

data GameEvent = EventKeyPress Player | EventExit | Continue String deriving (Eq, Show)

instance EqualsStringColors ColorPrinter where
    eq (ColorPrinter color str) = colorToString color == str

colorToString :: Color -> String
colorToString Red    = "red"
colorToString Blue   = "blue"
colorToString Green  = "green"
colorToString Yellow = "yellow"
colorToString White  = "white"

listOfColors :: [Color]
listOfColors = [Red, Blue, Green, Yellow, White]

listOfColorStrings :: [String]
listOfColorStrings = ["red", "blue", "green", "yellow", "white"]

charToBool :: Char -> Bool
charToBool 'y' = True
charToBool 'n' = False
charToBool _   = False

printInCenter :: String -> Int -> IO ()
printInCenter txt i = do
    (Just w) <- size
    clearFromCursorToScreenEnd
    setCursorPosition (height w `div` 2 + i) (width w `div` 2 - (length txt `div` 2))
    putStr txt

paintColorText :: ColorPrinter -> IO ()
paintColorText color = do
    (Just w) <- size
    setSGR [SetColor Background Vivid (getColor color)]
    let colorText = getColorString color
    setCursorPosition (height w `div` 2) (width w `div` 2 - (length colorText `div` 2))
    putStr (getColorString color)
    setSGR [Reset]

gameAction :: Char -> GameEvent
gameAction ch = case ch of
    'a' -> EventKeyPress (Player1 0)
    'l' -> EventKeyPress (Player2 0)
    'q' -> EventExit
    _   -> Continue "Please write correct symbol"

paint :: ColorPrinter -> StateT GameState IO GameEvent
paint color = do
    lift clearScreen
    lift $ paintColorText color
    lift $ printInCenter "Left player (a) or Right player (l) or q(Quit): " 7
    t <- lift $ ifReadyDo stdin getChar
    case t of
        Just str -> return $ gameAction str 
        Nothing  -> return $ Continue "Please write something"

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
    where f True = x >>= return . Just
          f _    = return Nothing


isWin :: Player -> ColorPrinter -> StateT GameState IO ()

isWin (Player1 _) color = do
    (GameState (Player1 p1, Player2 p2)) <- get
    if eq color
        then put (GameState (Player1 (succ p1), Player2 p2))
        else put (GameState (Player1 (pred p1), Player2 p2))

isWin (Player2 _) color = do
    (GameState (Player1 p1, Player2 p2)) <- get
    if eq color
        then put (GameState (Player1 p1, Player2 (succ p2)))
        else put (GameState (Player1 p1, Player2 (pred p2)))

gameLoop :: StateT GameState IO () 
gameLoop = do
    [x, y]       <- lift $ replicateM 2 $ randomRIO (0, pred $ length listOfColors)
    let color = ColorPrinter (listOfColors !! x) (listOfColorStrings !! y)
    event <- paint color
    case event of
        (EventKeyPress pl) -> isWin pl color
        (Continue txt)     -> lift $ print txt
        EventExit          -> mzero
        _                  -> lift $ print "LOL"
    get >>= (lift . putStrLn . show)
    lift $ threadDelay 3000000
    return ()

main :: IO (a, GameState)
main = runStateT (forever gameLoop) (GameState (Player1 0, Player2 0))

