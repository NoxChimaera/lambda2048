module L2048 where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
--import Data.Char (toLower)
import Data.List
--import System.IO
import System.Random
import Data.Maybe (fromJust)

{- Program -}
main :: IO ()
main = do
  seed <- randomIO
  let world = initialWorld seed
  {-
    play :: 
      Display - display mode, 
      Color - background colour, 
      Int - FPS,
      world,
      (world -> Picture) - render,
      (Event -> world -> world) - input handler,
      (Float -> world -> world) - updater, Float is dt
    -> Hell ()
  -}
  play
    (displayMode world)
    backgroundColor
    fps
    world
    render
    handleEvent
    update

{- play members -}
displayMode :: World -> Display
displayMode world = 
  -- Display.InWindow Title Size Position
  InWindow "HS2048" (resolution world) (0, 0)

backgroundColor :: Color
backgroundColor = white

fps :: Int
fps = 30

{- Creates new instance of world and adds one block to it -}
initialWorld :: Int -> World
initialWorld seed =
  initGrid NewWorld {
    resolution = (514, 514),
    blockSize = 128,
    isOver = False,
    gen = mkStdGen seed,
    score = 0,
    direction = None,
    board = [[]]
  }
  
initGrid :: World -> World
initGrid world =
  addTile $ addTile world {
    board = replicate 4 [0, 0, 0, 0]
  }

    --(rnd, g) = R.randomR (0, length xs - 1) $ gen world
  
addTile :: World -> World
addTile world = 
  let
    grid = board world
    candidates = getZeroes grid
    (rnd, g) = randomR (0, length candidates - 1) $ gen world
    pick = candidates !! rnd
    (rndval, g') = (randomR (0, 9) $ g)
    val = [2,2,2,2,2,2,2,2,2,4] !! rndval
    grid' = setSquare grid pick val
  in world { board = grid', gen = g' }

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = 
  filter (\(row, col) -> (grid !! row) !! col == 0) coordinates
  where 
    singleRow n = zip (replicate 4 n) [0..3]
    coordinates = concatMap singleRow [0..3]
  
setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid (row, col) val = 
  pre ++ [mid] ++ post
  where
    pre = take row grid
    mid = take col (grid !! row) ++ [val] ++ drop (col + 1) (grid !! row)
    post = drop (row + 1) grid

{- RENDER -}
{- Renders a world -}
render :: World -> Picture
render world = 
  let
    grid = renderGrid (board world) (blockSize world)
    wireframe = renderWireframe (blockSize world)
    pics = renderDebug world : union wireframe grid
  -- Picture.pictures :: [Picture] -> Picture
  in pictures pics

renderDebug :: World -> Picture
renderDebug world =
  translate (-300) (-280) $ color red (scale 0.1 0.1 (text $ show $ board world))
  
{- Renders a cell -}
renderCell :: Float -> Float -> Int -> Picture
renderCell x y size =
  let
    size' = fromIntegral size
  in
    translate x y (rectangleWire size' size')

{- Renders board wireframe -}
renderWireframe :: Int -> [Picture]
renderWireframe size =
  let
    s = fromIntegral size
    cells = [ (x, y) | x <- [-1.5..1.5], y <- [-1.5..1.5] ]
  in 
    fmap (\(x, y) -> renderCell (x*s) (y*s) size) cells

{- Renders a block with value on it -}
renderBlock :: (Float, Float) -> Int -> Int -> [Picture]
renderBlock (x, y) size value =
    let
      size' = fromIntegral size
      hs = (fromIntegral size)
    in [
      color orange (translate (x*hs) (y*hs) (rectangleSolid size' size')),
      translate ((x*hs) - 32) (y*hs) $ color white $ scale 0.25 0.25 $ text (show value)
    ]

{- Renders blocks -}    
renderRow :: (Int, [Int]) -> Int -> [Picture]
renderRow row scale =
  let 
    s' = fromIntegral scale
    i = fromIntegral $ fst row
    i' = i - 1.5
    row' = snd row
    
    v0 = row' !! 0
    v1 = row' !! 1
    v2 = row' !! 2
    v3 = row' !! 3
    
    p0 = if v0 > 0 then renderBlock (-1.5, i') s' v0 else []
    p1 = if v1 > 0 then renderBlock (-0.5, i') s' v1 else []
    p2 = if v2 > 0 then renderBlock (0.5, i') s' v2 else []
    p3 = if v3 > 0 then renderBlock (1.5, i') s' v3 else []
  in 
    concat [p0, p1, p2, p3]

renderGrid :: Grid -> Int -> [Picture]
renderGrid grid scale =
  let g' = [ (0, (grid !! 0)), (1, (grid !! 1)), (2, (grid !! 2)), (3, (grid !! 3)) ]
  in concat $ mapM (renderRow) g' scale
  
{- EVENT HANDLERS -}
{- Handles game events -}
handleEvent :: Event -> World -> World
handleEvent event world = 
  case event of
    EventResize newResolution -> 
      handleResize newResolution world
    EventKey key state _ _ -> 
      if isOver world
      then world
      else handleKey key state world
    _ -> world
 
handleResize :: (Int, Int) -> World -> World
handleResize newResolution world = world { resolution = newResolution }

handleKey :: Key -> KeyState -> World -> World
handleKey key state world = 
  case state of
    Down -> case key of
      SpecialKey KeyUp ->
        world { direction = North }
      SpecialKey KeyRight ->
        world { direction = East }
      SpecialKey KeyDown ->
        world { direction = South }
      SpecialKey KeyLeft ->
        world { direction = West }
      _ -> world { direction = None }
    _ -> world { direction = None }

{- UPDATE -}
update :: Float -> World -> World
update dt world =
  if isOver world
  then world
  else case direction world of
    West -> addTile world { direction = None, board = (moveLeft $ board world) }
    East -> addTile world { direction = None, board = (moveRight $ board world) }
    North -> addTile world { direction = None, board = (moveUp $ board world) }
    South -> addTile world { direction = None, board = (moveDown $ board world) }
    _ -> world
  
moveLeft :: Grid -> Grid
moveLeft grid =
  map merge grid
  
moveRight :: Grid -> Grid
moveRight grid =
  map (reverse . merge . reverse) grid
  
moveDown :: Grid -> Grid
moveDown grid =
  transpose $ moveLeft $ transpose grid
  
moveUp :: Grid -> Grid
moveUp grid =
  transpose $ moveRight $ transpose grid

merge :: [Int] -> [Int]
merge xs = 
  merged ++ padding
  where 
    padding = replicate (length xs - length merged) 0
    merged = combine $ filter (/= 0) xs
    combine (x:y:xs)  | x == y = x * 2 : combine xs
                      | otherwise = x : combine (y:xs)
    combine x = x

{--canMove :: Grid -> Bool
canMove grid =
  sum allChoices > 0
  where
    allChoices = map (length . getZeroes . flip move grid) directions
    directions = [Left, Right, Up, Down]
--} 
  
{- World -}
type Grid = [[Int]]
data World = NewWorld {
  resolution :: (Int, Int),
  blockSize :: Int,
  isOver :: Bool,
  gen :: StdGen,
  score :: Int,
  direction :: Direction,
  board :: Grid
} deriving (Read, Show)

size :: (Num a) => World -> a
size world =
  let (width, height) = resolution world
  in  fromIntegral (min width height)
    
data Direction = None | North | East | South | West deriving (Bounded, Enum, Eq, Ord, Read, Show)