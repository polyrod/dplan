{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Arrow
import Control.Monad hiding (when)
import Control.Monad.CSP
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.List
import Data.Maybe
import System.Environment

import Conf

type XSP r = ReaderT Env (CSP r)

runXSP = flip runReaderT

days :: [Day]
days = [Mo, Di, Mi, Do, Fr, Sa]

adistricts :: [District]
adistricts = [8, 10, 11, 14, 15, 13, 12, 16]

sortWorkers :: [Worker] -> [Worker]
sortWorkers = sortBy workerCmp
  where
    workerCmp (Worker _ _ (Stamm x) _) (Worker _ _ (Stamm y) _) = compare x y
    workerCmp (Worker _ _ Springer _) (Worker _ _ (Stamm y) _) = GT
    workerCmp (Worker _ _ (Stamm x) _) (Worker _ _ Springer _) = LT
    workerCmp (Worker _ bs1 Springer _) (Worker _ bs2 Springer _) =
      compare (length bs1) (length bs2)
    workerCmp (Worker _ _ Springer _) (Worker _ _ Vertreter _) = LT
    workerCmp (Worker _ _ Vertreter _) (Worker _ _ Springer _) = GT
    workerCmp (Worker _ _ Vertreter _) (Worker _ _ (Stamm y) _) = GT
    workerCmp (Worker _ bs1 Vertreter _) (Worker _ bs2 Vertreter _) =
      compare (length bs1) (length bs2)
    workerCmp (Worker _ _ (Stamm x) _) _ = LT
    workerCmp (Worker _ _ Joker _) _ = GT
    workerCmp _ (Worker _ _ Joker _) = LT

freeDay :: (Eq a, Num a) => Env -> District -> Int -> [Day]
freeDay env d kw =
  let b = head $ filter ((d ==) . num) (abezirke env)
      fdseq =
        take 1 $
        drop (offset b) $ drop kw $ drop (kwoffset env) $ cycle $ prog b
      montags d ds =
        if (isJust $ moz b) && elem (head ds) (mozfd b)
          then Mo : ds
          else ds
  in montags d $ fdseq

--buildCSP :: Int -> [Worker] -> ZSP r [[DV r Worker]]
buildCSP kw ws = mapM (bldDs kw ws) days

--bldDs :: Int -> [Worker] -> Day -> ZSP r [DV r Worker]
bldDs kw ws d = do
  env <- ask
  mapM (aDV env kw ws d . num) (abezirke env)

--aDV :: Int -> [Worker] -> Day -> District -> ZSP r (DV r Worker)
aDV env kw ws dy di = do
  env <- ask
  dvals <- constructDomain
  lift $ mkDV dvals
  where
    constructDomain = do
      env <- ask
      return $
        if dy == Mo
          then let b = head $ filter ((di ==) . num) (abezirke env)
               in if num b == di
                    then maybe (mondayZs di di) (mondayZs di) (moz b)
                    else []
          else let s = stammZ di
                   v = vertZ di
                   sp = sprinZ di
                   jo = jokerZ di
               in if dy `elem` freeDay env di kw
                    then if null sp
                           then v ++ jo
                           else sp ++ jo
                    else if null s
                           then v ++ jo
                           else s
    stammZ d = filter (\w -> Stamm d == rolle w) ws
    sprinZ d =
      let sps = filter (\w -> Springer == rolle w && elem d (bezirke w)) ws
      in if null sps
           then vertZ d
           else sps
    vertZ d = filter (\w -> Vertreter == rolle w && elem d (bezirke w)) ws
    jokerZ d = filter (\w -> Joker == rolle w && elem d (bezirke w)) ws
    mondayZs a b =
      let sx x =
            if notElem Mo $ freeDay env x kw
              then if null $ stammZ x
                     then vertZ x ++ jokerZ x
                     else stammZ x
              else []
          sa = sx a
          sb = sx b
          sp = sprinZ a `intersect` sprinZ b
      in nub $
         if null $ sa ++ sb
           then sp
           else sa ++ sb

--mycsp1 :: Env -> Int -> [Worker] -> [[Worker]]
--mycsp1 e kw workers = oneCSPSolution  $ runReaderT e $ mycsp' kw workers
--mycspA :: Env -> Int -> [Worker] -> [[[Worker]]]
mycspA e kw workers = allCSPSolutions $ runXSP e $ mycsp' kw workers

mycsp' :: Int -> [Worker] -> XSP r1 [[DV r1 Worker]]
mycsp' kw workers = do
  env <- ask
  dvs <- buildCSP kw workers
  let mo = head dvs
  let idx r d = r !! (fromJust $ elemIndex d (abezirke env))
  let mocons r da db
        | da == db = return ()
        | moz da == (Just $ num db) =
          lift $ do
            csp $
              putStrLn $
              "Asserting constraint for : " ++
              (show $ num da) ++
              " moz " ++ (show $ moz da) ++ " == " ++ (show $ num db)
            constraint2 (==) (idx mo da) (idx mo db)
        | moz da /= (Just $ num db) =
          lift $ do
            csp $
              putStrLn $
              "Asserting constraint for : " ++
              (show $ num da) ++
              " moz " ++ (show $ moz da) ++ " /= " ++ (show $ num db)
            constraint2 (/=) (idx mo da) (idx mo db)
  mapM (\d -> mapM (mocons mo d) $ abezirke env) $ abezirke env
  -- one day no duble worker for Di-Sa
  let noDubs a = length a == (length . nub) a
  lift $ mapM_ (constraint noDubs) $ tail dvs
  -- worker week bezirke with max cost  <=  wtage w
  let costMap = map (\dy -> map (cost dy) (abezirke env)) days
        where
          cost dy b =
            if dy == Mo
              then if isNothing (moz b)
                     then 1
                     else 0.5
              else 1
  forM_ workers $ \w ->
    lift $
    constraint
      (\xs ->
         fromIntegral (wtage w) >=
         sum (map snd $ filter (\p -> w == fst p) $ zip xs (concat costMap))) $
    concat dvs
  return dvs

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  when (null args) $ do
    putStrLn $ pn ++ " <KW> "
    error "Blah"
  let kw = read $ head args
  ws <- loadConf
  e <- loadConfig "appconf/mering.conf"
  case e of
    Just e' -> do
      let res = mycspA e' kw ws
      mapM_ (printResult (map num $ abezirke e') kw ws) res
      putStrLn $
        "\n     Es wurden " ++
        show (length res) ++ " mögliche Lösungen gefunden,\n\n"
    Nothing -> error "Abort. :("

printResult :: Show a => [District] -> a -> [Worker] -> [[Worker]] -> IO ()
printResult bs kw ws res = do
  putStrLn $ "\n\n\n"
  putStrLn $ pad 30 $ "     Wochenplan Kalenderwoche " ++ show kw
  putStrLn "     ------------------------------\n\n"
  putStrLn $ formatDplan bs res
  putStrLn $ formatWorkerPlan res ws $ genWorkerplan bs ws res

loadConf :: IO [Worker]
loadConf = do
  let remComments str = concat $ filter (\l -> "--" /= take 2 l) $ lines str
  str <- readFile "appconf/zsp.conf"
  let ws = remComments str
  let parsed = read ws :: [Worker]
  return $ sortWorkers parsed

workerFreq :: Foldable t => [Worker] -> t [Worker] -> [(Name, Int)]
workerFreq workers ws = map (name &&& cnt ws) workers
  where
    cnt ws w = length $ filter (== w) $ concat ws

formatDplan :: Foldable t => [District] -> [t Worker] -> [Char]
formatDplan bs wp =
  header ++
  concatMap
    (\(d, wplan) -> "\n     " ++ pad 5 (show d) ++ concatMap shwEintrag wplan)
    (zip days wp) ++
  "\n\n"
  where
    header = "          " ++ concatMap (pad 11 . show) bs ++ "\n"
    shwEintrag w = pad 11 $ name w

pad x s = s ++ pd
    where
      l = length s
      pd = replicate ((x - l)) ' '

formatWorkerPlan ::
     (Show a, Foldable t) => t [Worker] -> [Worker] -> [[(Day, a)]] -> [Char]
formatWorkerPlan res ws wps =
  header ++
  concat
    (for days $ \dy ->
       "\n     " ++
       pad 5 (show dy) ++
       (concat $
        for (zip ws wps) $
        (\(w, wp) ->
           let dists = shwEintrag $ map snd $ filter (\p -> fst p == dy) wp
               shwEintrag l =
                 pad 11 $
                 if null l
                   then "Frei"
                   else intercalate "+" $ map show l
           in dists))) ++
  footer
  where
    header = "          " ++ concatMap (pad 11 . name) ws ++ "\n"
    footer =
      "\n\n          " ++
      concatMap (pad 11 . show . snd) (workerFreq ws res) ++ "\n\n"

genWorkerplan :: Eq a => [District] -> [a] -> [[a]] -> [[(Day, District)]]
genWorkerplan ds ws dpl = map scn ws
  where
    scn w =
      concat $
      for (zip days dpl) $ \(dy, dvs) ->
        forMaybe (zip ds dvs) $ \(di, dv) ->
          if dv == w
            then Just (dy, di)
            else Nothing

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe as f = mapMaybe f as
