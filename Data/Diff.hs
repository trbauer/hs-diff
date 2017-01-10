module Data.Diff where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array
import Data.Char
import Data.List
import Debug.Trace
import Text.Printf
import qualified Data.IntMap.Strict as DIM

fillTable :: (a -> b -> Float) -> [a] -> [b] -> [[(Float,Float)]]
fillTable cmp as0 bs0 = []
  where -- as :: Array Int a
        as = listArray (0,length as0) as0
        -- bs :: Array Int b
        bs = listArray (0,length bs0) bs0


cmpQwerty :: Char -> Char -> Float
cmpQwerty c1 c2
  | c1 == c2 = 1
  | toUpper c1 == c2 || toUpper c2 == c1 = 0.8 -- shift key held
cmpQwerty c1 c2 = leftEdge rows
  where leftEdge [] = 0
        leftEdge ((x1:x2:xs):rs)
          | c1 == x1 && c2 == x2 = 0.9
          | otherwise = middle ((x2:xs):rs)
        middle ((x1:x2:x3:xs):rs) -- middle case
          | c1 == x2 && (c2 == x1 || c2 == x3) = 0.9
          | otherwise = middle ((x2:x3:xs):rs)
        middle ([x1,x2]:rs) -- right edge
          | c1 == x2 && c2 == x1 = 0.9
          | otherwise = leftEdge rs

rows :: [String]
rows =
  ["1234567890-=",
   "qwertyuiop",
   "asdfghjkl;'",
   "zxcvbnm,./"]

padR :: Int -> String -> String
padR k s = s ++ replicate (k - length s) ' '

forI :: [a] -> (Int -> a -> IO ()) -> IO ()
forI as func = forM_ (zip [0..] as) $ uncurry func

data Edit a b =
    Sim !a !b !Float
  | Del !a
  | Ins    !b
  deriving (Show,Eq)
editValue :: Edit a b -> Float
editValue (Sim _ _ dv) = dv
editValue _ = 0

type Path a b = [Edit a b]
type PathsC = [(Path Char Char, Float)]

type S = State SimCache


-- TODO: understand why filter is failing to prune out the 0's

simTest :: String -> String -> IO ()
simTest as bs = do
  let df = similarity cmpQwerty 0.0 0.0 as bs
  putStrLn $ fmtPaths df

fmtPaths :: PathsC -> String
fmtPaths ps = concatMap fmtPath ps
  where fmtPath :: (Path Char Char, Float) -> String
        fmtPath (ps,val) =
               printf "%4.2f" val ++ ":[" ++ intercalate ", " (map fmtElem ps) ++ "]" ++ "\n"
          where fmtElem :: Edit Char Char -> String
                fmtElem (Sim a b v) = "S:" ++ [a]
                fmtElem (Del a) = "D:" ++ [a]
                fmtElem (Ins b) = "I:" ++ [b]

type SimCache = DIM.IntMap PathsC
--     a_0  a_1  a_2  ...  a_n
-- b_0
-- b_1
-- b_2
-- ...
-- b_n
-- moves -> indicate deletion
-- moves V indicate insertion
-- diagnoal indicates similar
--
-- sim     - similarity function: if
-- pru_eq  - per-compare pruning factor.  if sim a b <= prune, we assume mismatch
-- pru_abs - prune out paths whose sum is <= this
--
similarity :: (Char -> Char -> Float) -> Float -> Float -> String -> String -> PathsC
similarity sim pru_eq pru_abs as bs =
    case DIM.lookup (pair 0 0) (snd (runState (fill 0 0 0) DIM.empty)) of
      Just ps -> reverse (sortOn snd ps)
  where pair :: Int -> Int -> Int
        pair a_ix b_ix = b_ix * as_n + a_ix
        -- unpair :: Int -> (Int,Int)
        -- unpair val = val`divMod`bs_n

        (as_n,bs_n) = (length as,length bs)
        (as_arr,bs_arr) = (listArray (0,as_n - 1) as, listArray (0,bs_n - 1) bs)

        fill :: Float -> Int -> Int -> S PathsC
        fill acc a_ix b_ix
          | a_ix == as_n && b_ix == bs_n =
            if acc <= pru_abs then (trace "PRUNE END" (return [])) else trace ("KEEPER: "++ show acc) (return [([],0)])
          | otherwise = body
          where body = do
                  m <- get
                  case DIM.lookup (pair a_ix b_ix) m of
                    Just ps -> do
                      -- traceS $ "memoized =>       " ++ show (maximum (map snd ps))
                      return ps -- memoized
                    Nothing -> do
                      -- traceS $ "fresh value: acc  " ++ show acc
                      let whenVal True x = x
                          whenVal False _ = return []
                          addElem ed ps = do
                            return $ map (\(eds,v) -> (ed:eds,v + editValue ed)) ps
                      -- similar
                      ss <- whenVal (a_ix < as_n && b_ix < bs_n) $ do
                        let a = as_arr ! a_ix
                            b = bs_arr ! b_ix
                            sim_ab = sim a b
                        if sim_ab <= pru_eq then return []
                          else fill (acc + sim_ab) (a_ix + 1) (b_ix + 1) >>= addElem (Sim a b sim_ab)
                      -- delete
                      ds <- whenVal (a_ix < as_n) $ do
                        let a = as_arr ! a_ix
                        fill acc (a_ix + 1) b_ix >>= addElem (Del a)
                      -- insert
                      is <- whenVal (b_ix < bs_n) $ do
                        let b = bs_arr ! b_ix
                        fill acc a_ix (b_ix + 1) >>= addElem (Ins b)
                      let new_ps = ss ++ ds ++ is -- should sort and prune
--                            where is1
--                                    -- better way would be to compute
--                                    | nn ds && nn is && tail ds == tail is = trace "PRUNING" $ []
--                                    | otherwise = is
--                                    where nn = not . null
                      -- traceS $ "storing value      " ++ show (maximum (map snd new_ps))
                      -- traceS $
                      --  "storing value: " ++ concatMap (\r -> "  - " ++ show r ++ "\n") new_ps
                      modify $ DIM.insert (pair a_ix b_ix) new_ps
                      return new_ps

                traceS :: String -> S ()
                traceS msg = trace (show (a_ix,b_ix) ++ ": " ++ msg) $ return ()

{-
                tryElem a b
                  case DIM.lookup (pair a_ix b_ix) m of
                    Just r -> do
                      traceS "memoized"
                      return r -- memoized
                    Nothing -> do
                      traceS "filling value"
                      dels <- tryDir 1 0 $ \a _ -> Del a
                      adds <- tryDir 1 0 $ \_ b -> Ins b
                      sims <- tryDir 1 1 $ \a b -> Sim a b (elemSim a b)
                      let res = (sims ++ dels ++ adds)
                      if null res
                        then traceS "storing empty list (0)"
                        else traceS $ "storing value: " ++ show (last (sort (map snd res ++ [0])))
                      -- traceS $
                      --  "storing value: " ++ concatMap (\r -> "  - " ++ show res ++ "\n") res
                      modify $ DIM.insert (pair a_ix b_ix) res
                      return res


                tryDir :: Int -> Int -> (Char -> Char -> Edit Char Char) -> S PathsC
                tryDir da db func
                  | a_ix + da > as_n || b_ix + db > bs_n = return []
                  | a_ix + da == 0 || b_ix + db == 0 = return []
                  | otherwise = do
                    let a = as_arr ! (a_ix + da)
                        b = bs_arr ! (b_ix + db)
                    ps <- fill (a_ix + da) (b_ix + db)
                    let ed = func a b
                    return $!
                      if null ps then [([ed],editValue ed)]
                        else map (\(eds,v) -> (ed:eds,v+editValue ed)) ps
-}



test :: String -> String -> IO ()
test xs ys = do
  putStrLn $ padR 8 "" ++ concatMap (\x -> " " ++ printf "%8c" x) xs
  forI ys $ \y_i y -> do
    putStr $ printf "%8c" y
    forI xs $ \x_i x -> do
      putStr $ " " ++ printf "%8.3f" (cmpQwerty x y)
    putStrLn ""

