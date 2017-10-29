{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.CSP
import Data.List
import Data.Maybe
import System.Environment

data Day
    = Mo
    | Di
    | Mi
    | Do
    | Fr
    | Sa
    deriving (Eq, Show)

type District = Int

type Name = String

type KW = Int

data Rolle
    = Stamm District
    | Springer
    | Vertreter
    | Joker
    deriving (Show, Eq, Read)

data Worker = Worker
    { name :: Name
    , bezirke :: [District]
    , rolle :: Rolle
    , wtage :: Int
    } deriving (Show, Read, Eq)

days = [Mo, Di, Mi, Do, Fr, Sa]

adistricts = [8, 10, 11, 14, 15, 13, 12, 16]

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

freeDay d kw = montags d $ take 1 $ fddist d kw
  where
    montags 8 ds = ds
    montags 13 ds = ds
    montags _ ds@[Mi] = Mo : ds
    montags _ ds@[Fr] = Mo : ds
    montags _ ds = ds
    fdseq = drop 1 $ cycle [Mo, Di, Mi, Do, Fr, Sa, Mo]
    fdseq' = drop 1 $ cycle [Mo, Di, Mi, Do, Fr, Sa]
    fddist 8 kw = drop 2 $ drop kw fdseq'
    fddist 10 kw = drop 0 $ drop kw fdseq
    fddist 11 kw = drop 1 $ drop kw fdseq
    fddist 14 kw = drop 4 $ drop kw fdseq
    fddist 15 kw = drop 5 $ drop kw fdseq
    fddist 13 kw = drop 1 $ drop kw fdseq'
    fddist 12 kw = drop 2 $ drop kw fdseq
    fddist 16 kw = drop 3 $ drop kw fdseq

buildCSP kw ws = mapM (bldDs kw ws) days

bldDs kw ws d = mapM (aDV kw ws d) adistricts

aDV kw ws dy di = mkDV constructDomain
  where
    constructDomain =
        if dy == Mo
            then case di of
                     8 -> mondayZs 8 8
                     10 -> mondayZs 10 11
                     11 -> mondayZs 11 10
                     14 -> mondayZs 14 15
                     15 -> mondayZs 15 14
                     13 -> mondayZs 13 13
                     12 -> mondayZs 12 16
                     16 -> mondayZs 16 12
            else let s = stammZ di
                     v = vertZ di
                     sp = sprinZ di
                     jo = jokerZ di
                 in if dy `elem` freeDay di kw
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
                if notElem Mo $ freeDay x kw
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

mycsp1 kw workers = oneCSPSolution $ mycsp' kw workers

mycspA kw workers = allCSPSolutions $ mycsp' kw workers

mycsp' kw workers = do
    dvs <- buildCSP kw workers
    let mo = head dvs
        eight = mo !! 0
        ten = mo !! 1
        eleven = mo !! 2
        fourteen = mo !! 3
        fivteen = mo !! 4
        thirteen = mo !! 5
        twelve = mo !! 6
        sixteen = mo !! 7
    constraint2 (==) ten eleven
    constraint2 (==) fourteen fivteen
    constraint2 (==) twelve sixteen
    constraint2 (/=) eight thirteen
    constraint2 (/=) eight ten
    constraint2 (/=) eight fourteen
    constraint2 (/=) eight twelve
    constraint2 (/=) ten fourteen
    constraint2 (/=) ten thirteen
    constraint2 (/=) fourteen thirteen
    constraint2 (/=) twelve thirteen
    constraint2 (/=) ten twelve
    constraint2 (/=) fourteen twelve
  -- one day no duble worker for Di-Sa
    let noDubs a = length a == (length . nub) a
    mapM_ (constraint noDubs) $ tail dvs
  -- worker week bezirke with max cost  <=  wtage w
    let costMap = map (\dy -> map (cost dy) adistricts) days
          where
            cost dy di =
                if dy == Mo
                    then if di == 8 || di == 13
                             then 1
                             else 0.5
                    else 1
    forM_ workers $ \w ->
        constraint
            (\xs ->
                 fromIntegral (wtage w) >=
                 sum
                     (map snd $
                      filter (\p -> w == fst p) $ zip xs (concat costMap))) $
        concat dvs
    return dvs

{-
  csp $ do
        forM_ (concat dvs) $ \v -> do
          d <- domain v
          dmn <- demons v
          putStrLn $ "NC: " ++ (show.length $ dmn) ++ " " ++ (show $ map name d) ++ "\n==========================="
          -}
main = do
    args <- getArgs
    pn <- getProgName
    when (null args) $ do
        putStrLn $ pn ++ " <KW> "
        error "Blah"
    let kw = read $ head args 
    !ws <- loadConf
    let res = mycspA kw ws
    mapM_ (printResult kw ws) res
    putStrLn $
        "\nEs wurden " ++
        show (length res) ++ " mögliche Lösungen gefunden,\n\n"

printResult kw ws res = do
    putStrLn $ "\n\n\nEin Wochenplan für die Kalenderwoche " ++ show kw
    putStrLn "-----------------------------------------------------\n\n"
    putStrLn $ formatDplan res
    putStrLn $ formatWorkerPlan res ws $ genWorkerplan ws res

loadConf = do
    let remComments str = concat $ filter (\l -> "--" /= take 2 l) $ lines str
    str <- readFile "appconf/zsp.conf"
    let ws = remComments str
    let parsed = read ws :: [Worker]
    return $ sortWorkers parsed

workerFreq workers ws = map (name &&& cnt ws) workers
  where
    cnt ws w = length $ filter (== w) $ concat ws

formatDplan wp =
    header ++
    concatMap
        (\(d, wplan) -> "\n" ++ pad 5 (show d) ++ concatMap shwEintrag wplan)
        (zip days wp) ++
    "\n\n"
  where
    header = "     " ++ concatMap (pad 11 . show) adistricts ++ "\n"
    shwEintrag w = pad 11 $ name w
    pad x s = s ++ post
      where
        post = replicate (x - length s) ' '

formatWorkerPlan res ws wps =
    header ++
    concat (
     for days $ \dy ->
         "\n" ++
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
    header = "     " ++ concatMap (pad 11 . name) ws ++ "\n"
    footer =
        "\n\n     " ++
        concatMap (pad 11 . show . snd) (workerFreq ws res) ++ "\n\n"
    pad x s = s ++ post
      where
        post = replicate (x - length s) ' '

genWorkerplan ws dpl = map scn ws
  where
    scn w =
        concat $
        for (zip days dpl) $ \(dy, dvs) ->
            forMaybe (zip adistricts dvs) $ \(di, dv) ->
                if dv == w
                    then Just (dy, di)
                    else Nothing

for as f = map f as

forMaybe as f = mapMaybe f as
