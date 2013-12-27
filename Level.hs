
module Level where
import Game
testLevel =
    mkStart newLevel{text="You are at screen 1. This is the first screen. Obviously."} where
        mkStart = (addNorthExit newLevel{text="Screen 2 is the second screen and is significantly less interesting."}).(addExit East newLevel{text="Screen 3"}) 

