{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
module AStarST where

import           Data.List (foldl', reverse)
import qualified Data.Map.Strict as Map
import           Data.Matrix.Unboxed (Matrix)
import qualified Data.Matrix.Unboxed as Mat
import           Data.Matrix.Unboxed.Mutable (MMatrix)
import qualified Data.Matrix.Unboxed.Mutable as MMat
import qualified Data.OrdPSQ as OrdPSQ
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V
import           Control.Monad.ST
import           Control.Monad (filterM, (<=<))
import           Control.Monad.Primitive (PrimMonad, PrimState)

import Maze
-- * A star algorithm

type TodoList = OrdPSQ Pos Distance (Distance, Maybe Pos)
type VisitedMap = Map.Map Pos (Distance, Maybe Pos)

data AStarState matrix = AStarState
    { maze :: !(matrix MazePixel)
    , todo :: !TodoList
    , goal :: !Pos
    , visited :: !VisitedMap
    , parents :: !(matrix (Bool, Pos))
    , done :: !(Maybe Pos)
    } -- TODO: State when done
 
-- deriving instance Show (matrix AStarCell) => Show (AStarState matrix)
deriving instance (Show (matrix MazePixel), Show (matrix (Bool, Pos))) => Show (AStarState matrix)
-- deriving instance Show (AStarState Matrix)


data AStarCell = Open !GVal | Closed !GVal | Unexplored | Unavailable | Goal
    -- deriving (Show, Eq)

instance Show AStarCell where
    show Unavailable = "#"
    show (Open _) = "O"
    show (Closed _) = "C"
    show Goal = "E"
    show Unexplored = "_"

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

findEnd :: Maze -> Pos
findEnd maze = onBoth (`div` V.length ends) $ V.foldl1' add ends
  where ends = findIdxMatrix (== End) maze

findIdxMatrix :: V.Unbox a => (a -> Bool) -> Matrix a -> V.Vector Pos
findIdxMatrix prec = V.map fst . V.filter (prec . snd) . Mat.flatten . Mat.imap (,)

findStart :: Pos -> Maze -> [(Pos, Distance, (Distance, Maybe Pos))]
findStart goal maze = [ (pos, heuristic goal pos , (0, Nothing)) | pos <- V.toList . findIdxMatrix (== Start) $ maze]

initAstar :: PrimMonad m => Maze -> m (AStarState (MMatrix (PrimState m)))
initAstar maze = do
    maze' <- Mat.thaw maze
    let (r,c) = Mat.dim maze
    parents <- Mat.thaw $ Mat.fromVector (r,c) $ V.replicate (r*c) (False, (0,0))
    pure $ AStarState
        { maze = maze' -- Mat.map go maze
        , todo = OrdPSQ.fromList $ findStart goal maze
        , goal = findEnd maze
        , visited = Map.empty
        , parents = parents
        , done = Nothing
        }
    where
        goal = findEnd maze
        -- go Free = Unexplored
        -- go Blocked = Unavailable
        -- go Start = Open 0
        -- go End = Unexplored
        -- go Other = Unavailable

pattern MinView :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
pattern MinView k p v w <- (OrdPSQ.minView -> Just (k, p, v, w))

-- stepAStar AStarState {maze, todo, goal}
--     | Nothing <- OrdPSQ.minView todo = Nothing
--     | Just (pos, _, (), todo') <- OrdPSQ.minView todo =
stepAStar :: PrimMonad m => AStarState (MMatrix (PrimState m)) -> 
                            m (Maybe (AStarState (MMatrix (PrimState m))))
-- stepAStar AStarState {maze, todo, goal}
--     | Nothing <- OrdPSQ.minView todo = Nothing
--     | Just (pos, _, (), todo') <- OrdPSQ.minView todo =
stepAStar state@AStarState {done = Just _} = pure $ Just state
stepAStar state@AStarState {maze, todo = MinView pos _ (cost,parent) todo, goal, visited} = do
    cell <- MMat.read maze pos
    if cell == End then
        pure $ Just state
            { todo = todo
            , visited = Map.insert pos (cost,parent) visited
            , done = Just pos
            }
    else do
        validNeighbors <- filterM (isAvailable maze) $ neighbors (MMat.dim maze) pos
        let todo'' = foldl' updateTodo todo validNeighbors
        MMat.write maze pos Avoid
        pure $ Just state
            { todo = todo''
            , visited = Map.insert pos (cost,parent) visited
            }
    where
        isAvailable :: PrimMonad m => MMatrix (PrimState m) MazePixel -> Pos -> m Bool
        isAvailable maze' pos' = do
            cell <- MMat.read maze' pos'
            pure $ case cell of
                Free -> True
                End -> True
                _ -> False
        -- validNeighbors = filter (`Map.notMember` visited) . filter isAvailable $ neighbors maze pos

        -- Insert a neighbor unless it was already there
        updateTodo :: OrdPSQ Pos Distance (Distance, Maybe Pos) -> Pos -> OrdPSQ Pos Distance (Distance, Maybe Pos)
        updateTodo queue pos' = case OrdPSQ.insertView pos' prio (cost', Just pos) queue of
            (Just (prio', _), _queue') | prio' <= prio -> queue  -- The old value was better
            (_, queue') -> queue'
            where
                cost' = cost + distance pos pos'
                prio = cost' + heuristic pos' goal

stepAStar _ = pure Nothing
            -- (\pos' -> maze Mat.! pos')
            -- Check out neighbors of pos
            -- Update neighbors in todo-list
            -- update in matrix
        -- minView :: (Ord k, Ord p) => OrdPSQ k p v -> Maybe (k, p, v, OrdPSQ k p v)

runAStar :: Maze -> Maybe [Pos]
runAStar maze = runST $ go =<< (Just <$> initAstar maze)
    where
        go :: Maybe (AStarState (MMatrix s)) -> ST s (Maybe [Pos])
        go (Just AStarState {visited, done = Just pos}) = pure . Just $ reverse $ unwind visited pos
        go (Just state) = go =<< stepAStar state
        go Nothing = pure Nothing

        unwind :: Map.Map Pos (Distance, Maybe Pos) -> Pos -> [Pos]
        unwind visited pos = case snd =<< Map.lookup pos visited of
            Nothing -> [pos]
            Just pos' -> pos : unwind visited pos'

-- traceState :: FilePath -> AStarState (MMatrix RealWorld) -> IO ()
-- traceState fileName mat =

convTrace :: Maze -> Maze
convTrace = Mat.map go
  where
    go :: MazePixel -> MazePixel
    go Avoid = End
    go x = x

runAStarTrace :: (Int -> Maze -> IO ()) -> Int -> Maze -> IO (Maybe [Pos])
runAStarTrace action interval = go 0 <=< fmap Just . initAstar
    where
        go :: Int -> Maybe (AStarState (MMatrix RealWorld)) -> IO (Maybe [Pos])
        go n (Just AStarState {visited, done = Just pos, maze}) = do
            action (( n`div`interval ) + 1) . convTrace =<< Mat.freeze maze
            pure . Just $ reverse $ unwind visited pos
        go n (Just state)
            | n `mod` interval == 0 = do
                stepped <- stepAStar state
                maybe (pure ()) (action (n`div`interval) . convTrace <=< Mat.freeze . maze ) stepped
                go (n+1) stepped
            | otherwise = go (n+1) =<< stepAStar state
        go _ Nothing = pure Nothing

        unwind :: Map.Map Pos (Distance, Maybe Pos) -> Pos -> [Pos]
        unwind visited pos = case snd =<< Map.lookup pos visited of
            Nothing -> [pos]
            Just pos' -> pos : unwind visited pos'

drawPath :: Maze -> [Pos] -> Maze
drawPath maze path = Mat.imap upd maze
    where
        pathSet = Set.fromList path

        upd :: Pos -> MazePixel -> MazePixel
        upd pos cell | Set.member pos pathSet = End
                     | otherwise = cell

drawAStar :: Maze -> Maybe Maze
drawAStar maze = drawPath maze <$> runAStar maze