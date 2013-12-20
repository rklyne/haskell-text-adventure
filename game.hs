import System.IO

type Item = String
type Items = [Item]
type Character = String

type Action = String -> Command
default_action :: Action
default_action text = print_message ("Unrecognised command: " ++ text)

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

describe_screen screen = text screen ++ "\n" ++ (exits_description screen)
    where
        exits_description screen = case exits screen of
            [] -> "There is no way out"
            xs -> "You can go: " ++ (comma_join xs)
        comma_join [] = ""
        comma_join [str] = str
        comma_join (str:strs) = str ++ ", " ++ (comma_join strs)
        exits :: Screen -> [String]
        exits screen = map show [d | d <- directions, check (exit screen d)]
        check Nothing = False
        check _ = True

data GameState = GameState {
    character:: Character,
    -- screens:: [Screen],
    current_screen:: Screen,
    inventory:: Items,
    game_over:: Bool
    }

type Command = (GameState -> IO GameState)

blank_state character start = GameState character start [] False

add_exit direction exit_screen screen = screen{exit=new_exit}
    where new_exit d = if d == direction then Just exit_screen else exit screen direction
add_north_exit = add_exit North
new_level = Screen{
    items=[],
    action=default_action,
    text="!!!! >> NO ROOM DESCRIPTION PROVIDED",
    exit=no_exit
    }
    where no_exit = \_ -> Nothing
test_level =
    mk_start new_level{text="You are at screen 1. This is the first screen. Obviously."} where
        mk_start = add_north_exit new_level{text="Screen 2 is the second screen and is significantly less interesting."}

-- Commands (game actions)

move_to :: Screen -> Command
move_to screen state = return (state{current_screen=screen})


move_in_direction :: Direction -> Command
move_in_direction direction state = exit_command ((exit (current_screen state)) direction) state
    where
    exit_command :: Maybe Screen -> Command
    exit_command screen = case screen of
        Nothing -> print_message "You can't go that way"
        Just screen -> move_to screen
move_north = move_in_direction North
move_south = move_in_direction South
move_west = move_in_direction East
move_east = move_in_direction East

print_message msg state = 
    do
        putStrLn msg
        return state

quit :: Command
quit state = return state{game_over=True}

-- The Game
play_game :: Screen -> IO ()
play_game level =
    do
        state <- starting_state
        final_state <- play_frames state
        putStrLn "  .~ fin ~."
    where
        starting_state = do
            name <- prompt "Enter your name> "
            return (blank_state name level)
        play_frames state = do
            new_state <- play_frame state
            if game_over new_state then
                return new_state
            else
                play_frames new_state
        play_frame state = do
            putStrLn (describe_screen (current_screen state))
            command <- prompt_for_command state
            command state

        prompt_for_command :: GameState -> IO Command
        prompt_for_command state = do
            input <- prompt (character state ++ "> ")
            return (parse_input input)
            where
                parse_input :: String -> Command
                parse_input input = (case input of
                    "north" -> move_north
                    "south" -> move_south
                    "east" -> move_east
                    "west" -> move_west
                    "n" -> move_north
                    "s" -> move_south
                    "e" -> move_east
                    "w" -> move_west
                    "q" -> quit
                    "quit" -> quit
                    otherwise -> action (current_screen state) input
                    )
        prompt message = do
            putStr message
            hFlush stdout
            getLine

main :: IO ()
main = play_game test_level

