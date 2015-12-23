module L2048 where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.List
import System.Random
import Data.Maybe (fromJust)

{- PROGRAM -}
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
  InWindow "Lambda2048" (resolution world) (0, 0)

backgroundColor :: Color
backgroundColor = white

fps :: Int
fps = 15

{- Creates new instance of world -}
initialWorld :: Int -> World
initialWorld seed =
  initGrid NewWorld {
    resolution = (514, 514),
    blockSize = 128,
    isOver = False,
    gen = mkStdGen seed,
    score = 0,
    direction = None,
    board = [[]],
    tick = True,
    emptyBlocks = 16
  }
  
{- Initializes game board and add two block -}  
initGrid :: World -> World
initGrid world =
  addTile $ (addTile world {
    board = replicate 4 [0, 0, 0, 0]
  }) { tick = True }

{- Adds block on board -}
addTile :: World -> World
addTile world = 
  if tick world
  then world { board = grid', gen = g', tick = False }
  else world
  where
    grid = board world
    candidates = getZeroes grid
    (rnd, g) = randomR (0, length candidates - 1) $ gen world
    pick = candidates !! rnd
    (rndval, g') = (randomR (0, 9) $ g)
    -- 90% 2, 10% - 4
    val = [2,2,2,2,2,2,2,2,2,4] !! rndval
    grid' = setSquare grid pick val

{- Finds empty places on game board -}
getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = 
  filter (\(row, col) -> (grid !! row) !! col == 0) coordinates
  where 
    singleRow n = zip (replicate 4 n) [0..3]
    coordinates = concatMap singleRow [0..3]
 
{- Moves block to the place -} 
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
    pics = concat [[renderDebug world], wireframe, grid, [renderGameOver world]]
  -- Picture.pictures :: [Picture] -> Picture
  in pictures pics

{- Renders game over message. Does it works? -}
renderGameOver :: World -> Picture
renderGameOver world =
  if isOver world
  then pictures [ color red (scale 0.2 0.2 (text "Game Over")) ] 
  else blank
  
{- Renders debug info -}
renderDebug :: World -> Picture
renderDebug world =
  let
    board' = show $ board world
    end' = show $ isOver world
    emp' = show $ emptyBlocks world
    debug = board' ++ " " ++ end' ++ " " ++ emp'
  in
    translate (-300) (-280) $ color red (scale 0.1 0.1 (text debug))
  
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
      size' = (fromIntegral size) - 4.0
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
    -- 'cause of (0, 0) in the centre of window
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

{- Renders game board -}
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
  else 
    let w' = move world
    in
      if isOver $ canMove $ w'
      then w'
      else addTile w'

{- Moves blocks -}      
move :: World -> World
move world =
  case direction world of
    West -> world { direction = None, board = (moveLeft $ board world), tick = True }
    East -> world { direction = None, board = (moveRight $ board world), tick = True }
    North -> world { direction = None, board = (moveUp $ board world), tick = True }
    South -> world { direction = None, board = (moveDown $ board world), tick = True }
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

{- Merges neighbour blocks with same values -}
merge :: [Int] -> [Int]
merge xs = 
  merged ++ padding
  where 
    padding = replicate (length xs - length merged) 0
    merged = combine $ filter (/= 0) xs
    combine (x:y:xs)  | x == y = x * 2 : combine xs
                      | otherwise = x : combine (y:xs)
    combine x = x

{- Game should end if there are no empty spaces on board. But it doesn't works. Maybe -}
canMove :: World -> World
canMove world =
  let 
    s = length (getZeroes $ board world)
  in 
    if s == 0
    then world { isOver = True, emptyBlocks = s }
    else world { emptyBlocks = s }
 
{- DATA -} 
type Grid = [[Int]]
data World = NewWorld {
  resolution :: (Int, Int),
  blockSize :: Int,
  isOver :: Bool,
  gen :: StdGen,
  score :: Int,
  direction :: Direction,
  board :: Grid,
  tick :: Bool,
  emptyBlocks :: Int
} deriving (Read, Show)

size :: (Num a) => World -> a
size world =
  let (width, height) = resolution world
  in  fromIntegral (min width height)
    
data Direction = None | North | East | South | West deriving (Bounded, Enum, Eq, Ord, Read, Show)