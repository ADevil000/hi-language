{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.Haskeline
import HW3.Parser
import HW3.Evaluator
import HW3.Pretty
import Text.Megaparsec.Error (errorBundlePretty)
import HW3.Base
import HW3.Action
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set, fromList)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> 
          do 
            case parse input of
              Left err -> outputStrLn $ errorBundlePretty err
              Right expr -> do
                val <- liftIO $ runHIO (eval expr) (fromList [AllowWrite, AllowRead, AllowTime])
                outputStrLn $ fromEitherToStr val
            loop
    fromEitherToStr :: Either HiError HiValue -> String
    fromEitherToStr (Left err) = show err
    fromEitherToStr (Right val) = show $ prettyValue val