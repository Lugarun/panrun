module Main where
import Options.Applicative
import System.IO
import System.Directory
import Data.List
import Data.Functor
import Data.Maybe
import Data.Either
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
  { position :: Int
  , pipeClass :: String
  , file :: FilePath
  } deriving (Show)

argParser :: Parser Panrun
argParser = Panrun
  <$> option auto
     ( long "line_number"
    <> short 'n'
    <> help "Line number in the code block" )
  <*> strOption
     ( long "pipe_class"
    <> short 'p'
    <> showDefault
    <> value "pipe"
    <> help "Name of pipe information in markdown" )
  <*> strArgument
     ( metavar "FILE"
    <> showDefault
    <> value "-"
    <> help "Input markdown file; with no FILE, or when FILE is -, read standard input " )

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

outputResults :: T.Text -> T.Text -> [Int] -> T.Text -> IO ()
outputResults lang pipe position code = do
  T.putStrLn lang
  T.putStrLn pipe
  T.putStrLn $ T.unwords $ map (T.pack . show) position
  T.putStrLn code

main :: IO ()
main = do
  args <- execParser opts

  -- read file or stdin
  fileExists <- doesFileExist (file args)
  str <-
    if file args == "-"
    then T.getContents
    else T.readFile (file args)

  -- get codeblocks via commonmark (for source mapping) and markdown (for meta data)
  let mdcb = runPure $ readMarkdown markdownOptions str <&> getCodeBlocks
  let cmdcb = runPure $ readCommonMark commonMarkOptions str <&> getCodeBlocks

  -- find codeblock at the given line number
  let codeBlockMask =
       cmdcb <&> map (containsPosition (position args))
  let cmsCodeBlock = either
       (const Nothing)
       (find (containsPosition (position args)))
       cmdcb
  let codeBlock = fromRight
       Nothing
       ((\m b -> find fst (zip m b)) `fmap` codeBlockMask <*> mdcb)
       <&> snd

  -- extract relevent metadata
  let pipe = codeBlock >>= (\x -> getCodeBlockAttrByKey x $ T.pack $ pipeClass args)
  let code = codeBlock >>= getCodeBlockCode
  let lang = codeBlock >>= getCodeBlockLang
  let position = cmsCodeBlock >>= getPosition

  -- format output
  fromJust $ outputResults <$> lang <*> pipe <*> position <*> code
  where
    opts = info (argParser <**> helper)
      ( fullDesc
     <> progDesc "Run and extract code blocks with meta data."
     <> header "panrun - a code runner for markdown" )
    commonMarkOptions = def
      { readerExtensions = extensionsFromList
        [ Ext_sourcepos
        , Ext_fenced_code_attributes
        , Ext_fenced_code_blocks ] }
    markdownOptions = def
      { readerExtensions = getDefaultExtensions $ T.pack "markdown" }


