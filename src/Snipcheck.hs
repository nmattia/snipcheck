{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Snipcheck where

import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe
import System.Process(readCreateProcess, shell)
import Text.Pandoc (Block(..))
import qualified Data.Text.IO as Text
import qualified Data.Map as Map

import qualified Text.Pandoc as Pandoc

data Sloppy a = Skip | Must a deriving (Show, Functor)

sloppyString :: String -> Sloppy String
sloppyString "..." = Skip
sloppyString str = Must str

checkSloppy :: Eq a => [a] -> [Sloppy a] -> Bool
checkSloppy (a:as) (Must a':as')
  | a == a' = checkSloppy as as'
  | otherwise = False
checkSloppy (a:as) as'@(Skip:Must a':as'')
  | a == a' = checkSloppy as as''
  | otherwise = checkSloppy as as'
checkSloppy as (Skip:Skip:as') = checkSloppy as (Skip:as')
checkSloppy [] (Must{}:_) = False
checkSloppy [] (Skip:as') = checkSloppy [] as'
checkSloppy [] [] = True
checkSloppy (_:_) [] = False
checkSloppy _ [Skip] = True

checkMarkdownFile :: FilePath -> IO ()
checkMarkdownFile fp = do
    content <- Text.readFile fp
    eres <- Pandoc.runIO $ do
      Pandoc.Pandoc meta blocks <- Pandoc.readMarkdown Pandoc.def content
      let
        sections = findSections meta
        blocks' =
          if null sections
          then blocks
          else filterBlocksBySectionName sections blocks
      forM_ blocks' check
    case eres of
      Right () -> pure ()
      Left e -> throwIO $ userError $ show e

data AcceptSection
  = GoodSection
  | BadSection
  | Dunno

filterBlocksBySectionName :: [String] -> [Pandoc.Block] -> [Pandoc.Block]
filterBlocksBySectionName secs = skipThese
  where
    skipThese, keepThese :: [Pandoc.Block] -> [Pandoc.Block]
    skipThese (b:bs) =
      case acceptSection b of
        GoodSection -> keepThese bs
        _ -> skipThese bs
    skipThese [] = []
    keepThese (b:bs) = b : case acceptSection b of
      BadSection -> skipThese bs
      _ -> keepThese bs
    keepThese [] = []
    acceptSection :: Pandoc.Block -> AcceptSection
    acceptSection (Pandoc.Header _ (hName,_,_) _)
      | hName `elem` secs = GoodSection
      | otherwise = BadSection
    acceptSection _ = Dunno

findSections :: Pandoc.Meta -> [String]
findSections (Pandoc.unMeta -> meta) =
  case Map.lookup "sc_check-sections" meta of
    Just (Pandoc.MetaList ss) -> join $ unMetaString <$> ss
    _ -> []
  where
    unMetaString :: Pandoc.MetaValue -> [String]
    unMetaString (Pandoc.MetaString s) =[s]
    unMetaString (Pandoc.MetaInlines is) = mapMaybe unMetaStr is
    unMetaString _ = []
    unMetaStr :: Pandoc.Inline -> Maybe String
    unMetaStr (Pandoc.Str s) = Just s
    unMetaStr _ = Nothing

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

check :: MonadIO m => Pandoc.Block -> m ()
check (CodeBlock (typ, classes, kvs) content)
  | "shell" `elem` classes = do
      let Right cmds = extractCommands content
      forM_ cmds $ \(cmd, expected) -> do
        actual <- (fmap trim . lines) <$> liftIO (readCreateProcess (shell cmd) "")
        let expected' = (sloppyString . trim) <$> expected
        unless (checkSloppy actual expected') $ error $ mconcat
          [ "Couldnt match expected ", show expected'
          , " with " <> show actual
          ]
  | otherwise = liftIO $ print (typ, classes, kvs)
check _ = return ()


extractCommands :: String -> Either String [(String, [String])]
extractCommands str = go (lines str)
  where
    go :: [String] -> Either String [(String, [String])]
    go (l:ls) | Just cmd <- toCommand l =
      let (output, rest) = break isCommand ls
      in ((cmd,output):) <$> go rest
              | otherwise = Left $ "Expected a command, got " <> l
    go [] = Right []
    toCommand :: String -> Maybe String
    toCommand ('$':cmd) = Just cmd
    toCommand _ = Nothing
    isCommand :: String -> Bool
    isCommand = isJust . toCommand


someFunc :: IO ()
someFunc = putStrLn "someFunc"
