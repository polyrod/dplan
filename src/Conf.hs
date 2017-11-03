{-# LANGUAGE ScopedTypeVariables #-}

module Conf where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Char
import Data.ConfigFile
import Data.List
import Data.List.Split

data Day
  = Mo
  | Di
  | Mi
  | Do
  | Fr
  | Sa
  deriving (Eq, Show, Read)

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

data Env = Env
  { abezirke :: [Bezirk]
  , kwoffset :: Int
  } deriving (Read, Show)

data Bezirk = Bezirk
  { num :: District
  , prog :: [Day]
  , moz :: Maybe Int
  , mozfd :: [Day]
  , offset :: Int
  } deriving (Read, Show, Eq)

loadConfig :: String -> IO (Maybe Env)
loadConfig fn = do
  rv <-
    runExceptT $ do
      cp <-
        join $
        liftIO $ readfile (emptyCP {accessfunc = interpolatingAccess 10}) fn
      let x = cp
      let secs = sections x
      fds :: [String] <- forM secs $ \s -> get x s "progression"
      mfds :: [String] <- forM secs $ \s -> get x s "mozfds"
      moz :: [Maybe Int] <-
        forM secs $ \s ->
          if has_option x s "moz"
            then Just <$> get x s "moz"
            else pure Nothing
      os :: [Int] <- forM secs $ \s -> get x s "versatz"
      let bzs =
            for (zip5 (map (dropWhile (not . isDigit)) $ secs) fds moz mfds os) $ \(s, fd, moz, mozfds, os) ->
              Bezirk (read s) (fds2prog fd) moz (fds2prog mozfds) os
      --liftIO $ forM bzs $ \b -> putStrLn $ show b
      kwo :: Int <- get x "DEFAULT" "kwversatz"
      let e = Env bzs kwo
      --liftIO $ print e
      return $ show e
  print rv
  case rv of
    Left x -> do
      return Nothing
    Right x -> return $ Just $ (read x)

fds2prog :: String -> [Day]
fds2prog s = map (read) $ splitOn "," s

for :: [a] -> (a -> b) -> [b]
for as f = map f as
