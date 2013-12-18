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
    north_exit:: Maybe Screen,
    south_exit:: Maybe Screen,
    east_exit:: Maybe Screen,
    west_exit:: Maybe Screen,

    action:: Action
    }

describe_screen screen = text screen ++ "\n" ++ (exits_description screen)
    where
        exits_description screen = case exits screen of
            [] -> "There is no way out"
            xs -> "You can go: " ++ (comma_join xs)
        comma_join [] = ""
        comma_join [str] = str
        comma_join (str:strs) = str ++ ", " ++ (comma_join strs)
        exits :: Screen -> [String]
        exits screen = let
                (n, s, e, w) = (
                    north_exit screen,
                    south_exit screen,
                    east_exit screen,
                    west_exit screen
                    )
                check Nothing = True
                check _ = False
            in concat [
                if check n then [] else ["North"],
                if check s then [] else ["South"],
                if check e then [] else ["East"],
                if check w then [] else ["West"]
                ]

data GameState = GameState {
    character:: Character,
    -- screens:: [Screen],
    current_screen:: Screen,
    inventory:: Items,
    game_over:: Bool
    }

type Command = (GameState -> IO GameState)

blank_state character start = GameState character start [] False

-- Level builder
-- type LevelSetup = ([Screen], Screen)
-- data IdGenerator = IdGenerator Integer
-- apply_id_space (State s f) = result
--     where (result, state) = f (IdGenerator 1)

-- next_id = State (\(IdGenerator g) -> (g, (IdGenerator (succ g))))

add_north_exit exit screen = screen{north_exit=(Just exit)}
new_level = Screen{
    items=[],
    action=default_action,
    text="!!!! >> NO ROOM DESCRIPTION PROVIDED",
    north_exit=no_exit,
    south_exit=no_exit,
    east_exit=no_exit,
    west_exit=no_exit
    }
    where no_exit = Nothing
test_level =
    mk_start new_level{text="Screen 1"} where
        mk_start = add_north_exit new_level{text="screen 2"}

-- Commands (game actions)

move_to :: Screen -> Command
move_to screen state = return (state{current_screen=screen})
exit_command screen = case screen of
    Nothing -> print_message "You can't go that way"
    Just screen -> move_to screen

move_north state = (exit_command . north_exit) (current_screen state) state
move_south state = (exit_command . south_exit) (current_screen state) state
move_west state = (exit_command . west_exit) (current_screen state) state
move_east state = (exit_command . east_exit) (current_screen state) state

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
            putStr "Enter your name> "
            hFlush stdout
            name <- getLine
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
            input <- getLine
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

main :: IO ()
main = play_game test_level

