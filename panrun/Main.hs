module Main where

import Options.Applicative
import System.IO
import System.Directory
import Data.List
import Data.Functor
import Data.Maybe
import Text.Read
import Text.Regex
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Definition
import Text.Pandoc.Readers
import Text.Pandoc.Class
import qualified Data.Text.IO as T
import qualified Data.Text as T

data Panrun = Panrun
  { file :: FilePath
  , position :: Int
  } deriving (Show)

argParser :: Parser Panrun
argParser = Panrun
  <$> strOption
     ( long "file"
    <> short 'f'
    <> help "You guessed it, the markdown file" )
  <*> option auto
     ( long "line_number"
    <> short 'n'
    <> help "Line number in the code block" )

getPosition :: Block -> Maybe [Int]
getPosition (CodeBlock (id, classes, kv) content) = range
  where
    range :: Maybe [Int]
    range = posAttr <&> T.unpack >>= (matchRegex (mkRegex "([0-9]+):1-([0-9]+):1")) >>= (mapM readMaybe)
    posAttr :: Maybe T.Text
    posAttr = find (\(n, _) -> n == T.pack "data-pos") kv <&> snd

containsPosition :: Int -> Block -> Bool
containsPosition pos (CodeBlock (id, classes, kv) content) = maybe False (\r -> pos >= r!!0 && pos <= r!!1) range
  where
    range :: Maybe [Int]
    range = posAttr <&> T.unpack >>= (matchRegex (mkRegex "([0-9]+):1-([0-9]+):1")) >>= (mapM readMaybe)
    posAttr :: Maybe T.Text
    posAttr = find (\(n, _) -> n == T.pack "data-pos") kv <&> snd

getCodeBlocks :: Pandoc -> [Block]
getCodeBlocks doc = do
  query extractCode doc
  where
    extractCode :: Block -> [Block]
    extractCode (CodeBlock as s) = [(CodeBlock as s)]
    extractCode _ = []

getCodeBlockAttrByKey :: Block -> T.Text -> Maybe T.Text
getCodeBlockAttrByKey (CodeBlock (_, _, kv) _) k = find ((== k) . fst) kv <&> snd
getCodeBlockAttrByKey _ _ = Nothing

getCodeBlockLang :: Block -> Maybe T.Text
getCodeBlockLang (CodeBlock (_, cl, _) _) = listToMaybe cl
getCodeBlockLang _ = Nothing

getCodeBlockCode :: Block -> Maybe T.Text
getCodeBlockCode (CodeBlock (_, _, _) c) = Just c
getCodeBlockCode _ = Nothing

outputResults :: T.Text -> T.Text -> T.Text -> IO ()
outputResults lang pipe code = do
  T.putStrLn $ lang
  T.putStrLn $ pipe
  T.putStrLn $ code

ok :: Panrun -> IO()
ok args = do
  fileExists <- doesFileExist (file args)
  str <- T.readFile (file args)
  let md = runPure $ readMarkdown def{ readerExtensions = getDefaultExtensions $ T.pack "markdown" } str
  let cmd = runPure $ readCommonMark def {readerExtensions = extensionsFromList [Ext_sourcepos, Ext_fenced_code_attributes, Ext_fenced_code_blocks]} str
  let codeBlockMask = cmd <&> getCodeBlocks <&> map (containsPosition (position args))
  let codeBlocks = md <&> getCodeBlocks
  let codeBlock = (either (\_ -> Nothing) id $ (\m b -> (find fst (zip m b))) `fmap` codeBlockMask <*> codeBlocks )<&> snd
  let pipe = codeBlock >>= (\x -> getCodeBlockAttrByKey x $ T.pack "pipe")
  let code = codeBlock >>= getCodeBlockCode
  let lang = codeBlock >>= getCodeBlockLang
  fromJust $ outputResults <$> lang <*> pipe <*> code

main :: IO ()
main = execParser opts >>= ok
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Run and extract code blocks with meta data."
     <> header "panrun - a code runner for markdown" )


