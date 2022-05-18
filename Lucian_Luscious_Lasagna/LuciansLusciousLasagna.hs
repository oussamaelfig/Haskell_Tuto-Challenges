module Lucian_Luscious_Lasagna.LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven = 40

-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes numberOfLayers =   2 * numberOfLayers

-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes numberOfLayers numMinutesLasagnaInOven = preparationTimeInMinutes numberOfLayers + numMinutesLasagnaInOven
