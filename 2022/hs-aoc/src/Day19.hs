{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Day19 (part1, part2) where

import Text.Regex.TDFA ((=~), getAllTextMatches)

data Blueprint = Blueprint
  { i :: Int
  , oreCostOre :: Int
  , clayCostOre :: Int
  , obsCostOre :: Int
  , obsCostClay :: Int
  , geoCostOre :: Int
  , geoCostObs :: Int } deriving (Show, Eq)

data State = State
  { minsLeft :: Int
  , ore :: Int
  , oreRobots :: Int
  , clay :: Int
  , clayRobots :: Int
  , obs :: Int
  , obsRobots :: Int
  , geo :: Int
  , geoRobots :: Int
  } deriving (Show, Eq)

parseLine :: String -> Blueprint
parseLine s = Blueprint { i = a, oreCostOre = b, clayCostOre = c, obsCostOre = d, obsCostClay = e, geoCostOre = f, geoCostObs = g } where
  [a,b,c,d,e,f,g] = map read (getAllTextMatches (s =~ "[0-9]+") :: [String])

mostGeodes :: Blueprint -> State -> Int
mostGeodes _ (State { minsLeft = 0, geo }) = geo
mostGeodes bp state = maximum $ map (mostGeodes bp) (moves state bp) where
  -- possible next steps
  moves :: State -> Blueprint -> [State]
  moves st@(State { .. }) (Blueprint { .. }) = [onlyMine] ++ tryOreRobot ++ tryClayRobot ++ tryObsRobot ++ tryGeoRobot where
    newOre = ore + oreRobots
    newClay = clay + clayRobots
    newObs = obs + obsRobots
    newGeo = geo + geoRobots
    onlyMine = st 
      { minsLeft = minsLeft - 1
      , ore = newOre
      , clay = newClay
      , obs = newObs
      , geo = newGeo }
    tryOreRobot = if ore >= oreCostOre then [onlyMine
      { oreRobots = oreRobots + 1, ore = newOre - oreCostOre }] else []
    tryClayRobot = if ore >= clayCostOre then [onlyMine
      { clayRobots = clayRobots + 1, ore = newOre - clayCostOre}] else []
    tryObsRobot = if ore >= obsCostOre && clay >= obsCostClay then [onlyMine
      { obsRobots = obsRobots + 1, ore = newOre - obsCostOre, clay = newClay - obsCostClay }] else []
    tryGeoRobot = if ore >= geoCostOre && obs >= geoCostObs then [onlyMine
      { geoRobots = geoRobots + 1, ore = newOre - geoCostOre, obs = newObs - geoCostObs }] else []
  
-- I think I have a solution here but it's too slow without memoization, which
-- is very hard to do in Haskell!

initState :: State
initState = State
  { minsLeft = 24
  , ore = 0
  , oreRobots = 1
  , clay = 0
  , clayRobots = 0
  , obs = 0
  , obsRobots = 0
  , geo = 0
  , geoRobots = 0
  }

part1 :: IO ()
part1 = do
  -- input <- readFile "../input/day19.txt"
  input <- readFile "../input/day19-example.txt"
  let bs = map parseLine (lines input)
  let maxGs = map ((flip mostGeodes) initState) bs
  print $ sum $ map (\(b, g) -> i b * g) (zip bs maxGs)  

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day19.txt"
  -- input <- readFile "../input/day19-example.txt"
  print $ "part2"