
module Game where

import System.IO
import Data.Char

type Item = String
type Items = [Item]
type Character = String

type Action = String -> Command
defaultAction :: Action
defaultAction text = printMessage ("Unrecognised command: " ++ text)

data Screen = Screen {
    items:: Items,
    text:: String,
    exit:: Direction -> Maybe Screen,
    action:: Action
    }

data Direction = North | South | East | West deriving (Show, Eq, Enum)
directions = [North .. ]
opposite North = South
opposite East = West
opposite South = North
opposite West = East

describeScreen screen = text screen ++ "\n" ++ (exitsDescription screen)
    where
        exitsDescription screen = case exits screen of
            [] -> "There is no way out"
            xs -> "You can go: " ++ (commaJoin xs)
        commaJoin [] = ""
        commaJoin [str] = str
        commaJoin (str:strs) = str ++ ", " ++ (commaJoin strs)
        exits :: Screen -> [String]
        exits screen = map show [d | d <- directions, check (exit screen d)]
        check Nothing = False
        check _ = True

data GameState = GameState {
    character:: Character
    -- , screens:: [Screen]
    , currentScreen:: Screen
    , inventory:: Items
    , gameOver:: Bool
    , output:: String
    }

type Command = (GameState -> GameState)

blankState character start = GameState character start [] False ""


-- Commands (game actions)

moveTo :: Screen -> Command
moveTo screen state = state{currentScreen=screen}


moveInDirection :: Direction -> Command
moveInDirection direction state = exitCommand ((exit here) direction) state
    where
    here = currentScreen state
    exitCommand :: Maybe Screen -> Command
    exitCommand screen = case screen of
        Nothing -> printMessage "You can't go that way"
        Just screen -> moveTo screen
moveNorth = moveInDirection North
moveSouth = moveInDirection South
moveWest = moveInDirection West
moveEast = moveInDirection East

printMessage msg state = state{output=(concat [(output state), "\n", msg])}

quit :: Command
quit state = state{gameOver=True}


---------------------------------
-- The parser

parseCommand :: String -> Command
parseCommand input = (case (map toLower input) of
    "north" -> moveNorth
    "south" -> moveSouth
    "east" -> moveEast
    "west" -> moveWest
    "n" -> moveNorth
    "s" -> moveSouth
    "e" -> moveEast
    "w" -> moveWest
    "q" -> quit
    "quit" -> quit
    otherwise -> (\state -> (action (currentScreen state) input state))
    )

-- The Game
playGame :: Screen -> IO ()
playGame level =
    do
        state <- startingState
        finalState <- playFrames state
        putStrLn "  .~ fin ~."
    where
        startingState :: IO GameState
        startingState = do
            name <- prompt "Enter your name> "
            return (blankState name level)
        playFrames :: GameState -> IO GameState
        playFrames state = do
            newState <- playFrame state
            if gameOver newState then
                return newState
            else
                playFrames newState
        playFrame state = do
            putStrLn (output state)
            putStrLn (describeScreen (currentScreen state))
            command <- promptForCommand (state{output=""})
            return $ command state

        -- promptForCommand :: GameState -> IO Command
        promptForCommand state = do
            input <- prompt (character state ++ "> ")
            return (parseCommand input)
        prompt message = do
            putStr message
            hFlush stdout
            getLine


--------------------------------
-- Level definition

addExit direction exitScreen screen = newScreen
    where
    newScreen = screen{exit=newExit}
    newExit d =
        if d == direction
        then Just modifiedExitScreen
        else exit screen d
    modifiedExitScreen = exitScreen{exit=returnExit}
    returnExit d =
        if d == (opposite direction)
        then Just newScreen
        else exit exitScreen d
addNorthExit = addExit North
newLevel = Screen{
    items=[],
    action=defaultAction,
    text="!!!! >> NO ROOM DESCRIPTION PROVIDED",
    exit=noExit
    }
    where noExit = \_ -> Nothing

