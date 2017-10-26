{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Exception
import           Control.Monad.State
import           Control.Concurrent.MVar
import           UI.HSCurses.Curses as Ncurses
import           UI.HSCurses.CursesHelper
-- import           qualified System.Console.ANSI as ANSI
import           System.IO
import           System.Random

class EqualsStringColors a where
    eq :: a -> Bool

data ColorPrinter = ColorPrinter { getColor       :: BackgroundColor
                                 , getColorString :: String }
                    deriving (Show)

data Player = Player1 Int | Player2 Int
            deriving (Eq, Show)

data GameState = GameState (Player, Player)
               deriving (Eq, Show)

data GameEvent = EventKeyPress Player | EventExit | Continue String deriving (Eq, Show)

instance EqualsStringColors ColorPrinter where
    eq (ColorPrinter color str) = colorToString color == str

colorToString :: BackgroundColor -> String
colorToString DarkRedB    = "red"
colorToString DarkBlueB   = "blue"
colorToString DarkGreenB  = "green"
colorToString WhiteB      = "white"

listOfColors :: [BackgroundColor]
listOfColors = [DarkRedB, DarkGreenB, DarkBlueB, {-  DarkCyanB, -} WhiteB]

listOfColorStrings :: [String]
listOfColorStrings = ["red", "blue", "green", "white"]

printInCenter :: Ncurses.Window -> String -> Int -> IO ()
printInCenter w txt i = do
    (height, width) <- scrSize  
    move (height `div` 2 + i) (width `div` 2 - (length txt `div` 2))
    wRefresh w
    wAddStr w txt
    wRefresh w

paintSquares :: Window -> Int -> IO ()
paintSquares w l = scrSize >>= (\size -> mapM_ (paintSquare size) [-1, -1, 0, 0, 1, 1]) >> wRefresh w
    where 
        paintSquare (height, width) x = do
            move (height `div` 2 + x) (width `div` 2 - (l `div` 2) - l)
            wAddStr w emptyText
            when (x /= 0) $ do 
                move (height `div` 2 + x) (width `div` 2 - (l `div` 2))
                wAddStr w emptyText
            move (height `div` 2 + x) (width `div` 2 - (l `div` 2) + l )
            wAddStr w emptyText
        emptyText = replicate l ' '


paintColorText :: Ncurses.Window -> ColorPrinter -> IO ()
paintColorText w color = do
    (height, width) <- scrSize  
    [cursesStyle] <- convertStyles [Style DefaultF (getColor color)]
    wSetStyle w cursesStyle
    wRefresh w
    let colorText = getColorString color
    printInCenter w colorText 0
    paintSquares w (length colorText)
    wResetStyle w

gameAction :: Char -> GameEvent
gameAction ch = case ch of
    'a' -> EventKeyPress (Player1 0)
    'l' -> EventKeyPress (Player2 0)
    'q' -> EventExit
    _   -> Continue "Please write correct symbol"

paint :: Ncurses.Window -> ColorPrinter -> IO ()
paint w color = do
    wclear w
    wRefresh w
    paintColorText w color
    printInCenter w "Left player (a) or Right player (l) or q(Quit): " 6

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

printScore :: Ncurses.Window -> GameState -> IO () 
printScore w (GameState (p1, p2)) = do
    printInCenter w ("***Score***") 9
    printInCenter w ("Player1: " ++ (show p1) ++ ", Player2: " ++ (show p2)) 10

gameLoop :: Ncurses.Window -> StateT GameState IO ()
gameLoop w = do
    [x, y] <- lift $ replicateM 2 $ randomRIO (0, pred $ length listOfColors)
    let color = ColorPrinter (listOfColors !! x) (listOfColorStrings !! y)
    lift $ paint w color
    get >>= lift . printScore w
    answer <- lift $ race (timer 1) getAnswer
    case answer of 
        (Left _) -> gameLoop w
        (Right event) -> 
            case event of
                (EventKeyPress pl) -> isWin pl color
                (Continue txt)     -> do 
                    lift $ printInCenter w "***Error***" 15
                    lift $ printInCenter w (txt ++ " (Tap any char or wait 2 sec)") 16
                    lift $ race (timer 2) anyChar
                    return ()
                EventExit          ->  mzero
    
main :: IO ()
main = do
    w <- initScr
    startColor
    cursSet CursorInvisible
    useDefaultColors
    catch (runStateT (forever $ gameLoop w) (GameState (Player1 0, Player2 0))) (\e -> do 
                                                                                    let _ = e :: IOException
                                                                                    return ((), GameState (Player1 0, Player1 0)))
    endWin
    return ()
