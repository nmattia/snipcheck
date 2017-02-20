module Snipcheck where

import Control.Monad
import Data.Maybe
import Data.Monoid
import System.Process(readCreateProcess, shell)
import Text.Pandoc (Block(..))

import qualified Text.Pandoc as Pandoc

checkMarkdownFile :: FilePath -> IO ()
checkMarkdownFile fp = do
  content <- readFile fp
  let (Right (Pandoc.Pandoc _meta blocks)) = Pandoc.readMarkdown Pandoc.def content
  forM_ blocks check

check :: Pandoc.Block -> IO ()
check (CodeBlock (typ, classes, kvs) content)
  | "shell" `elem` classes = do
      let Right cmds = extractCommands content
      forM_ cmds $ \(cmd, expected) -> do
        actual <- lines <$> readCreateProcess (shell cmd) ""
        unless (actual == expected) $ error $ mconcat
          [ "Couldnt match expected ", show expected
          , " with " <> show actual
          ]
  | otherwise = print (typ, classes, kvs)
check _ = return ()

extractCommands :: String -> Either String [(String, [String])]
extractCommands str = go (lines str)
  where
    go :: [String] -> Either String [(String, [String])]
    go (l:ls) | Just cmd <- toCommand l =
      let (output, rest) = span (not . isCommand) ls
      in ((cmd,output):) <$> (go rest)
              | otherwise = Left $ "Expected a command, got " <> l
    go [] = Right []
    toCommand :: String -> Maybe String
    toCommand ('$':cmd) = Just cmd
    toCommand _ = Nothing
    isCommand :: String -> Bool
    isCommand = isJust . toCommand


someFunc :: IO ()
someFunc = putStrLn "someFunc"
