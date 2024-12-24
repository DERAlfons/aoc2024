{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains a collection of generalized graph search algorithms,
-- for when you don't want to explicitly represent your data as a graph. The
-- general idea is to provide these algorithms with a way of generating "next"
-- states, a way of generating associated information, a way of determining
-- when you have found a solution, and an initial state.
module Day16.Search (
  -- * Searches
  dijkstra,
  dijkstraAssoc,
  -- * Monadic Searches
  -- $monadic
  --dijkstraM,
  --dijkstraAssocM,
  -- * Utility
  incrementalCosts,
  incrementalCostsM,
  pruning,
  pruningAssoc,
  pruningM,
  pruningAssocM,
  QList (..),
  qhead
  ) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity(..))
import Control.Monad (filterM, zipWithM)

data QList a = S | D a (QList a) | Q (QList a) (QList a)

qhead :: QList a -> a
qhead (D a _) = a

qmap :: (a -> b) -> QList a -> QList b
qmap _ S = S
qmap f (D a rest) = D (f a) (qmap f rest)
qmap f (Q q1 q2) = Q (qmap f q1) (qmap f q2)

-- | @dijkstra next cost found initial@ performs a shortest-path search over
-- a set of states using Dijkstra's algorithm, starting with @initial@,
-- generating neighboring states with @next@, and their incremental costs with
-- @costs@. This will find the least-costly path from an initial state to a
-- state for which @found@ returns 'True'. Returns 'Nothing' if no path to a
-- solved state is possible.
--
-- === Example: Making change problem, with a twist
--
-- >>> :{
-- -- Twist: dimes have a face value of 10 cents, but are actually rare
-- -- misprints which are worth 10 dollars
-- countChange target =
--   dijkstra (add_coin `pruning` (> target)) true_cost  (== target) 0
--   where
--     coin_values = [(25, 25), (10, 1000), (5, 5), (1, 1)]
--     add_coin amt = map ((+ amt) . snd) coin_values
--     true_cost low high =
--       case lookup (high - low) coin_values of
--         Just val -> val
--         Nothing -> error $ "invalid costs: " ++ show high ++ ", " ++ show low
-- :}
--
-- >>> countChange 67
-- Just (67,[1,2,7,12,17,42,67])
dijkstra :: (Foldable f, Num cost, Ord cost, Ord state)
  => (state -> f state)
  -- ^ Function to generate list of neighboring states given the current state
  -> (state -> state -> cost)
  -- ^ Function to generate transition costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dijkstra' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe (cost, QList state)
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstra next cost found initial =
  -- This API to Dijkstra's algorithm is useful when the state transition
  -- function and the cost function are logically separate.
  -- It is implemented by using @dijkstraAssoc@ with appropriate mapping of
  -- arguments.
  dijkstraAssoc next' found initial
  where
    next' st = map (\new_st -> (new_st, cost st new_st)) $
               Foldable.toList (next st)

-- | @dijkstraAssoc next found initial@ performs a shortest-path search over
-- a set of states using Dijkstra's algorithm, starting with @initial@,
-- generating neighboring states with associated incremenal costs with
-- @next@. This will find the least-costly path from an initial state to a
-- state for which @found@ returns 'True'. Returns 'Nothing' if no path to a
-- solved state is possible.
dijkstraAssoc :: (Num cost, Ord cost, Ord state)
  => (state -> [(state, cost)])
  -- ^ function to generate list of neighboring states with associated
  -- transition costs given the current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'dijkstraAssoc' returns the
  -- shortest path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe (cost, QList state)
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstraAssoc next found initial =
  -- This API to Dijkstra's algoritm is useful in the common case when next
  -- states and their associated transition costs are generated together.
  --
  -- Dijkstra's algorithm can be viewed as a generalized search, with the search
  -- container being a heap, with the states being compared without regard to
  -- cost, with the shorter paths taking precedence over longer ones, and with
  -- the stored state being (cost so far, state).
  -- This implementation makes that transformation, then transforms that result
  -- back into the desired result from @dijkstraAssoc@
  unpack <$>
    generalizedSearch emptyLIFOHeap snd leastCostly next' (found . snd)
      (0, initial)
  where
    next' (old_cost, st) =
      (\(new_st, new_cost) -> (new_cost + old_cost, new_st))
        <$> (next st)
    unpack S = (0, S)
    unpack packed_states = (fst . qhead $ packed_states, qmap snd packed_states)


-- $monadic
-- Note that for all monadic searches, it is up to the user to ensure that
-- side-effecting monads do not logically change the structure of the graph.
-- For example, if the list of neighbors is being read from a file, the user
-- must ensure that those values do not change between reads.

{-
-- | @dijkstraM@ is a monadic version of 'dijkstra': it has support for monadic
-- @next@, @cost@, and @found@ parameters.
dijkstraM :: (Monad m, Foldable f, Num cost, Ord cost, Ord state)
  => (state -> m (f state))
  -- ^ Function to generate list of neighboring states given the current state
  -> (state -> state -> m cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'dijkstraM' returns the
  -- shortest path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstraM nextM costM foundM initial =
  fmap2 unpack $ generalizedSearchM emptyLIFOHeap snd leastCostly nextM'
    (foundM . snd) (0, initial)
  where
    nextM' (old_cost, old_st) = do
      new_states <- Foldable.toList <$> nextM old_st
      incr_costs <- sequence $ costM old_st <$> new_states
      let new_costs = (+ old_cost) <$> incr_costs
      return $ zip new_costs new_states
    unpack [] = (0, [])
    unpack packed_states = (fst . last $ packed_states, map snd packed_states)
-}


{-
-- | @dijkstraAssocM@ is a monadic version of 'dijkstraAssoc': it has support
-- for monadic @next@ and @found@ parameters.
dijkstraAssocM :: (Monad m, Num cost, Ord cost, Ord state)
  => (state -> m [(state, cost)])
  -- ^ Function to generate list of neighboring states with associated
  -- transition costs given the current state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. 'dijkstraM' returns the
  -- shortest path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which
  -- satisfies the given predicate
dijkstraAssocM nextM foundM initial =
  fmap2 unpack $ generalizedSearchM emptyLIFOHeap snd leastCostly nextM'
    (foundM . snd) (0, initial)
  where
    nextM' (old_cost, old_st) = do
      new_states <- nextM old_st
      return $ map (\(x, y) -> (y, x)) new_states
    unpack [] = (0, [])
    unpack packed_states = (fst . last $ packed_states, map snd packed_states)
-}


-- | @incrementalCosts cost_fn states@ gives a list of the incremental costs
-- going from state to state along the path given in @states@, using the cost
-- function given by @cost_fn@. Note that the paths returned by the searches
-- in this module do not include the initial state, so if you want the
-- incremental costs along a @path@ returned by one of these searches, you
-- want to use @incrementalCosts cost_fn (initial : path)@.
--
-- === Example: Getting incremental costs from dijkstra
--
-- >>> import Data.Maybe (fromJust)
--
-- >>> :{
-- cyclicWeightedGraph :: Map.Map Char [(Char, Int)]
-- cyclicWeightedGraph = Map.fromList [
--   ('a', [('b', 1), ('c', 2)]),
--   ('b', [('a', 1), ('c', 2), ('d', 5)]),
--   ('c', [('a', 1), ('d', 2)]),
--   ('d', [])
--   ]
-- start = (0, 0)
-- end = (0, 2)
-- cost a b = fromJust . lookup b $ cyclicWeightedGraph Map.! a
-- :}
--
-- >>> incrementalCosts cost ['a', 'b', 'd']
-- [1,5]
incrementalCosts ::
  (state -> state -> cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states in the `states` list, so it is safe to have
  -- this function be partial for non-neighboring states.
  -> [state]
  -- ^ A path, given as a list of adjacent states, along which to find the
  -- incremental costs
  -> [cost]
  -- ^ List of incremental costs along given path
incrementalCosts cost_fn states = zipWith cost_fn states (tail states)

-- | @incrementalCostsM@ is a monadic version of 'incrementalCosts': it has
-- support for a monadic @const_fn@ parameter.
incrementalCostsM ::
  (Monad m) =>
  (state -> state -> m cost)
  -- ^ Function to generate list of costs between neighboring states. This is
  -- only called for adjacent states in the `states` list, so it is safe to have
  -- this function be partial for non-neighboring states.
  -> [state]
  -- ^ A path, given as a list of adjacent states, along which to find the
  -- incremental costs
  -> m [cost]
  -- ^ List of incremental costs along given path
incrementalCostsM costM states = zipWithM costM states (tail states)


-- | @next \`pruning\` predicate@ streams the elements generate by @next@ into a
-- list, removing elements which satisfy @predicate@. This is useful for the
-- common case when you want to logically separate your search's `next` function
-- from some way of determining when you've reached a dead end.
--
-- === Example: Pruning a Set
--
-- >>> import qualified Data.Set as Set
--
-- >>> ((\x -> Set.fromList [0..x]) `pruning` even) 10
-- [1,3,5,7,9]
--
-- === Example: depth-first search, avoiding certain nodes
--
-- >>> import qualified Data.Map as Map
--
-- >>> :{
-- graph = Map.fromList [
--   ('a', ['b', 'c', 'd']),
--   ('b', [undefined]),
--   ('c', ['e']),
--   ('d', [undefined]),
--   ('e', [])
--   ]
-- :}
--
-- >>> dfs ((graph Map.!) `pruning` (`elem` "bd")) (== 'e') 'a'
-- Just "ce"
pruning ::
  (Foldable f)
  => (a -> f a)
  -- ^ Function to generate next states
  -> (a -> Bool)
  -- ^ Predicate to prune on
  -> (a -> [a])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
next `pruning` predicate =
  (filter (not . predicate) . Foldable.toList) <$> next


-- | @pruningAssoc@ is a version of 'pruning' that works with the `Assoc` variants of searches.
pruningAssoc ::
  (Foldable f)
  => (state -> f (state, cost))
  -- ^ Function to generate next states
  -> ((state, cost) -> Bool)
  -- ^ Predicate to prune on
  -> (state -> [(state, cost)])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
next `pruningAssoc` predicate =
  (filter (not . predicate) . Foldable.toList) <$> next

-- | @pruningM@ is a monadic version of 'pruning': it has support for monadic
-- @next@ and @predicate@ parameters
pruningM ::
  (Monad m, Foldable f)
  => (a -> m (f a))
  -- ^ Function to generate next states
  -> (a -> m Bool)
  -- ^ Predicate to prune on
  -> (a -> m [a])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
pruningM nextM predicateM a = do
  next_states <- nextM a
  filterM (fmap not . predicateM) $ Foldable.toList next_states

-- | @pruningAssocM@ is a monadic version of 'pruningAssoc': it has support for monadic
-- @next@ and @predicate@ parameters
pruningAssocM ::
  (Monad m, Foldable f)
  => (state -> m (f (state, cost)))
  -- ^ Function to generate next states
  -> ((state, cost) -> m Bool)
  -- ^ Predicate to prune on
  -> (state -> m [(state, cost)])
  -- ^ Version of @next@ which excludes elements satisfying @predicate@
pruningAssocM nextM predicateM a = do
  next_states <- nextM a
  filterM (fmap not . predicateM) $ Foldable.toList next_states


-- | A @SearchState@ represents the state of a generalized search at a given
-- point in an algorithms execution. The advantage of this abstraction is that
-- it can be used for things like bidirectional searches, where you want to
-- stop and start a search part-way through.
data SearchState container stateKey state = SearchState {
  current :: state,
  queue :: container,
  visited :: Set.Set stateKey,
  paths :: Map.Map stateKey (QList state)
  }

-- | Workhorse simple search algorithm, generalized over search container
-- and path-choosing function. The idea here is that many search algorithms are
-- at their core the same, with these details substituted. By writing these
-- searches in terms of this function, we reduce the chances of errors sneaking
-- into each separate implementation.
generalizedSearch ::
  (Foldable f, SearchContainer container, Ord stateKey, Elem container ~ state)
  => container
  -- ^ Empty @SearchContainer@
  -> (state -> stateKey)
  -- ^ Function to turn a @state@ into a key by which states will be compared
  -- when determining whether a state has be enqueued and / or visited
  -> (QList state -> QList state -> Int)
  -- ^ Function @better old new@, which when given a choice between an @old@ and
  -- a @new@ path to a state, returns True when @new@ is a "better" path than
  -- old and should thus be inserted
  -> (state -> f state)
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. @generalizedSearch@ returns a
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> Maybe (QList state)
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
generalizedSearch empty mk_key better next found initial = runIdentity $
  generalizedSearchM empty mk_key better (Identity . next) (Identity . found) initial

-- | @nextSearchState@ moves from one @searchState@ to the next in the
-- generalized search algorithm
nextSearchStateM ::
  (Monad m, Foldable f, SearchContainer container, Ord stateKey,
   Elem container ~ state)
  => (QList state -> QList state -> Int)
  -> (state -> stateKey)
  -> (state -> m (f state))
  -> SearchState container stateKey state
  -> m (Maybe (SearchState container stateKey state))
nextSearchStateM better mk_key nextM old = do
  (new_queue, new_paths) <- new_queue_paths_M
  let new_state_May = mk_search_state new_paths <$> pop new_queue
  case new_state_May of
    Just new_state ->
      if mk_key (current new_state) `Set.member` visited old
      then nextSearchStateM better mk_key nextM new_state
      else return (Just new_state)
    Nothing -> return Nothing
  where
    mk_search_state new_paths (new_current, remaining_queue) = SearchState {
      current = new_current,
      queue = remaining_queue,
      visited = Set.insert (mk_key new_current) (visited old),
      paths = new_paths
      }
    new_queue_paths_M =
      List.foldl' update_queue_paths (queue old, paths old)
        <$> nextM (current old)
    update_queue_paths (old_queue, old_paths) st =
      if mk_key st `Set.member` visited old
      then (old_queue, old_paths)
      else
        case Map.lookup (mk_key st) old_paths of
          Just old_path -> case better old_path (D st steps_so_far) of
            1 ->  (q', ps')
            -1 -> (old_queue, old_paths)
            0 -> (q', pss old_path)
          Nothing -> (q', ps')
        where
          steps_so_far = paths old Map.! mk_key (current old)
          q' = push old_queue st
          ps' = Map.insert (mk_key st) (D st steps_so_far) old_paths
          pss op = Map.insert (mk_key st) (spos op) old_paths
          spos (D _ rest) = D st (Q steps_so_far rest)


-- | @generalizedSearchM@ is a monadic version of generalizedSearch
generalizedSearchM ::
  (Monad m, Foldable f, SearchContainer container, Ord stateKey,
   Elem container ~ state)
  => container
  -- ^ Empty @SearchContainer@
  -> (state -> stateKey)
  -- ^ Function to turn a @state@ into a key by which states will be compared
  -- when determining whether a state has be enqueued and / or visited
  -> (QList state -> QList state -> Int)
  -- ^ Function @better old new@, which when given a choice between an @old@ and
  -- a @new@ path to a state, returns True when @new@ is a "better" path than
  -- old and should thus be inserted
  -> (state -> m (f state))
  -- ^ Function to generate "next" states given a current state
  -> (state -> m Bool)
  -- ^ Predicate to determine if solution found. @generalizedSearch@ returns a
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> m (Maybe (QList state))
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
generalizedSearchM empty mk_key better nextM foundM initial = do
  let initial_state =
        SearchState initial empty (Set.singleton $ mk_key initial)
        (Map.singleton (mk_key initial) S)
  end_May <- findIterateM (nextSearchStateM better mk_key nextM)
    (foundM . current) initial_state
  return $ fmap get_steps end_May
  where
    get_steps search_st = paths search_st Map.! mk_key (current search_st)


newtype LIFOHeap k a = LIFOHeap (Map.Map k [a])


emptyLIFOHeap :: LIFOHeap k a
emptyLIFOHeap = LIFOHeap Map.empty


-- | The @SearchContainer@ class abstracts the idea of a container to be used in
-- @generalizedSearch@
class SearchContainer container where
  type Elem container
  pop :: container -> Maybe (Elem container, container)
  push :: container -> Elem container -> container

instance SearchContainer (Seq.Seq a) where
  type Elem (Seq.Seq a) = a
  pop s =
    case Seq.viewl s of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push s a = s Seq.|> a

instance SearchContainer [a] where
  type Elem [a] = a
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

instance Ord k => SearchContainer (LIFOHeap k a) where
  type Elem (LIFOHeap k a) = (k, a)
  pop (LIFOHeap inner)
    | Map.null inner = Nothing
    | otherwise = case Map.findMin inner of
      (k, [a]) -> Just ((k, a), LIFOHeap $ Map.deleteMin inner)
      (k, a : _) -> Just ((k, a), LIFOHeap $ Map.updateMin (Just . tail) inner)
      (_, []) -> pop (LIFOHeap $ Map.deleteMin inner)
                 -- Logically, this should never happen
  push (LIFOHeap inner) (k, a) = LIFOHeap $ Map.insertWith (++) k [a] inner


-- | @findIterateM@ is a monadic version of @findIterate@
findIterateM :: Monad m => (a -> m (Maybe a)) -> (a -> m Bool) -> a -> m (Maybe a)
findIterateM nextM foundM initial = do
  found <- foundM initial
  if found
  then return $ Just initial
  else nextM initial >>= maybe (return Nothing) (findIterateM nextM foundM)


-- | @leastCostly paths_a paths_b@ is a utility function to be used with
-- 'dijkstra'-like functions. It returns True when the cost of @paths_a@
-- is less than the cost of @paths_b@, where the total costs are the first
-- elements in each tuple in each path
leastCostly :: Ord a => QList (a, b) -> QList (a, b) -> Int
leastCostly (D (cost_a, _) _) (D (cost_b, _) _)
  | cost_b <  cost_a =  1
  | cost_b >  cost_a = -1
  | cost_b == cost_a =  0
-- logically this never happens, because if you have a
-- zero-length path a point, you already visited it
-- and thus do not consider other paths to it
leastCostly S _ = -1
-- logically this never happens, because you cannot find
-- a new zero-length path to a point
leastCostly _ S = 1


-- | This is just a convenience function which @fmap@s two deep
fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap

