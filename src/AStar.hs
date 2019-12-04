{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
module AStar where

import           Data.List (foldl', reverse)
import qualified Data.Map.Strict as Map
import           Data.Matrix.Unboxed (Matrix)
import qualified Data.Matrix.Unboxed as Mat
import qualified Data.OrdPSQ as OrdPSQ
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V

import Maze
-- * A star algorithm

-- | The distance from the starting position and (if available) the position of the parent pixel
type DistParent = (Distance, Maybe Pos)
type TodoList = OrdPSQ Pos Distance DistParent
type VisitedMap = Map.Map Pos DistParent

-- | The state of the A* algorithm
data AStarState matrix = AStarState
    { maze :: !(matrix MazePixel) -- ^ The maze we are trying to solve
    , todo :: !TodoList           -- ^ A priority queue of the open set
    , goal :: !Pos                -- ^ The goal we are trying to reach
    , visited :: !VisitedMap      -- ^ The closed set that we have already visited
    , done :: !(Maybe Pos)        -- ^ Just endPos if we are done, Nothing
    }
 
-- deriving instance Show (matrix AStarCell) => Show (AStarState matrix)
deriving instance Show (matrix MazePixel) => Show (AStarState matrix)

-- | Unused: State of a single cell
data AStarCell = Open !GVal | Closed !GVal | Unexplored | Unavailable | Goal
    -- deriving (Show, Eq)

instance Show AStarCell where
    show Unavailable = "#"
    show (Open _) = "O"
    show (Closed _) = "C"
    show Goal = "E"
    show Unexplored = "_"

-- | Illustrate the current state of the A* algorithm
showAStar :: AStarState Matrix -> Matrix MazePixel
showAStar AStarState {maze, todo, visited} = Mat.imap fun maze
    where
        fun :: Pos -> MazePixel -> MazePixel
        fun pos _ | Map.member pos visited = End
        fun _pos End = End
        fun pos _ | OrdPSQ.member pos todo = Start
        fun _ x = x
-- showAStar :: AStarState Matrix -> Matrix AStarCell
-- showAStar AStarState {maze, todo, visited} = Mat.imap fun maze
--     where
--         fun :: Pos -> MazePixel -> AStarCell
--         fun pos _ | Just (cost, _parent) <- Map.lookup pos visited = Closed cost
--         fun pos End = Goal
--         fun pos _ | Just (_prio, (cost,_parent)) <- OrdPSQ.lookup pos todo = Open cost
--         fun pos Free = Unexplored
--         fun pos _ = Unavailable

-- | Find the middle of all end-points
findEnd :: Maze -> Pos
findEnd maze = onBoth (`div` V.length ends) $ V.foldl1' add ends
  where ends = findIdxMatrix (== End) maze

findIdxMatrix :: V.Unbox a => (a -> Bool) -> Matrix a -> V.Vector Pos
findIdxMatrix prec = V.map fst . V.filter (prec . snd) . Mat.flatten . Mat.imap (,)

-- | Find all start points
findStart :: Pos -> Maze -> [(Pos, Distance, (Distance, Maybe Pos))]
findStart goal maze = [ (pos, heuristic goal pos , (0, Nothing)) | pos <- V.toList . findIdxMatrix (== Start) $ maze]

-- | Initialize the A* algorithm
initAstar :: Maze -> AStarState Matrix
initAstar maze = AStarState
    { maze = maze -- Mat.map go maze
    , todo = OrdPSQ.fromList $ findStart goal maze
    , goal = goal
    , visited = Map.empty
    , done = Nothing
    }
    where
        goal = findEnd maze
        -- go Free = Unexplored
        -- go Blocked = Unavailable
        -- go Start = Open 0
        -- go End = Unexplored
        -- go Other = Unavailable

-- | A pattern for extracting the smallest element from a priority search queue
pattern MinView :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
pattern MinView k p v w <- (OrdPSQ.minView -> Just (k, p, v, w))

-- | Run a single step of A*
stepAStar :: AStarState Matrix -> Maybe (AStarState Matrix)
stepAStar state@AStarState {maze, todo = MinView pos _ (cost,parent) todo, goal, visited} 
    | maze Mat.! pos == End =
        Just state
            { todo = todo
            , visited = Map.insert pos (cost,parent) visited
            , done = Just pos
            }
    | otherwise =
        Just state
            { todo = todo''
            , visited = Map.insert pos (cost,parent) visited
            }
    where
        isAvailable pos' =
            case maze Mat.! pos' of
                Free -> True
                End -> True
                _ -> False
        validNeighbors = filter (`Map.notMember` visited) . filter isAvailable $ neighbors 1 (Mat.dim maze) pos
        todo'' = foldl' updateTodo todo validNeighbors

        -- Insert a neighbor unless it was already there
        updateTodo :: OrdPSQ Pos Distance (Distance, Maybe Pos) -> Pos -> OrdPSQ Pos Distance (Distance, Maybe Pos)
        updateTodo queue pos' = case OrdPSQ.insertView pos' prio (cost', Just pos) queue of
            (Just (prio', _), _queue') | prio' <= prio -> queue  -- The old value was better
            (_, queue') -> queue'
            where
                cost' = cost + distance pos pos'
                prio = cost' + heuristic pos' goal

stepAStar _ = Nothing

-- | Solve a maze using A* and return the optimal path
runAStar :: Maze -> Maybe [Pos]
runAStar maze = go (Just $ initAstar maze)
    where
        go :: Maybe (AStarState Matrix) -> Maybe [Pos]
        go (Just AStarState {visited, done = Just pos}) = Just $ reverse $ unwind visited pos
        go (Just state) = go (stepAStar state)
        go Nothing = Nothing

        unwind :: Map.Map Pos (Distance, Maybe Pos) -> Pos -> [Pos]
        unwind visited pos = case snd =<< Map.lookup pos visited of
            Nothing -> [pos]
            Just pos' -> pos : unwind visited pos'

-- | Solve a maze with A* and draw the solution in the maze
drawAStar :: Maze -> Maybe Maze
drawAStar maze | Just path <- runAStar maze =
    let
        pathSet = Set.fromList path
        upd :: Pos -> MazePixel -> MazePixel
        upd pos cell | Set.member pos pathSet = End
                     | otherwise = cell
    in  Just $ Mat.imap upd maze
drawAStar _ = Nothing