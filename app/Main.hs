{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Exception
import           Control.Monad.State
import           Control.Concurrent.MVar
import           UI.HSCurses.Curses as Ncurses
import           qualified System.Console.ANSI as ANSI
import           System.Console.Terminal.Size
import           System.IO
import           System.Random

class EqualsStringColors a where
    eq :: a -> Bool

data ColorPrinter = ColorPrinter { getColor       :: ANSI.Color
                                 , getColorString :: String }
                    deriving (Show)

data Player = Player1 Int | Player2 Int
            deriving (Eq, Show)

data GameState = GameState (Player, Player)
               deriving (Eq, Show)

data GameEvent = EventKeyPress Player | EventExit | Continue String deriving (Eq, Show)

instance EqualsStringColors ColorPrinter where
    eq (ColorPrinter color str) = colorToString color == str

colorToString :: ANSI.Color -> String
colorToString ANSI.Red    = "red"
colorToString ANSI.Blue   = "blue"
colorToString ANSI.Green  = "green"
colorToString ANSI.Yellow = "yellow"
colorToString ANSI.White  = "white"

listOfColors :: [ANSI.Color]
listOfColors = [ANSI.Red, ANSI.Blue, ANSI.Green, ANSI.Yellow, ANSI.White]

listOfColorStrings :: [String]
listOfColorStrings = ["red", "blue", "green", "yellow", "white"]

printInCenter :: Show a => a -> Int -> IO ()
printInCenter txt i = do
    let str = show txt
    (Just w) <- size
    ANSI.clearFromCursorToScreenEnd
    ANSI.setCursorPosition (height w `div` 2 + i) (width w `div` 2 - (length str `div` 2))
    putStr str

paintColorText :: ColorPrinter -> IO ()
paintColorText color = do
    (Just w) <- size
    ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid (getColor color)]
    let colorText = getColorString color
    ANSI.setCursorPosition (height w `div` 2) (width w `div` 2 - (length colorText `div` 2))
    putStr (getColorString color)
    ANSI.setSGR [ANSI.Reset]

gameAction :: Char -> GameEvent
gameAction ch = case ch of
    'a' -> EventKeyPress (Player1 0)
    'l' -> EventKeyPress (Player2 0)
    'q' -> EventExit
    _   -> Continue "Please write correct symbol"

-- gameAction :: Char - 

-- paint :: ColorPrinter -> StateT GameState IO GameEvent
paint :: ColorPrinter -> IO ()
paint color = do
    ANSI.clearScreen
    paintColorText color
    printInCenter "Left player (a) or Right player (l) or q(Quit): " 7

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

timer :: Int -> IO ()
timer seconds = do 
    threadDelay (seconds * 1000000)

getAnswer :: IO GameEvent
getAnswer = do 
    t <- ifReadyDo stdin getChar
    case t of
        Just str -> return $ gameAction str
        Nothing  -> getAnswer

anyChar :: IO ()
anyChar = getChar >> return ()

printScore :: GameState -> IO () 
printScore (GameState (p1, p2)) = printInCenter ("Player1: " ++ (show p1) ++ ", Player2: " ++ (show p2)) 10

gameLoop :: StateT GameState IO ()
gameLoop = do
    [x, y] <- lift $ replicateM 2 $ randomRIO (0, pred $ length listOfColors)
    let color = ColorPrinter (listOfColors !! x) (listOfColorStrings !! y)
    lift $ paint color
    get >>= lift . printScore
    answer <- lift $ race (timer 3) getAnswer
    case answer of 
        (Left _) -> gameLoop 
        (Right event) -> 
            case event of
                (EventKeyPress pl) -> isWin pl color
                (Continue txt)     -> do 
                    lift $ printInCenter txt 9
                    lift $ race (timer 2) anyChar
                    return ()
                EventExit          ->  mzero
    
main :: IO ()
main = do
    ANSI.clearScreen
    catch (runStateT (forever gameLoop) (GameState (Player1 0, Player2 0))) (\e -> do 
                                                                                    let exc = e :: IOException
                                                                                    return ((), GameState (Player1 0, Player1 0)))
    return ()

