{-# LANGUAGE GADTs #-}

module Lib
    ( entry
    ) where

import Options.Applicative
import System.IO
import System.Directory
import Data.Time.Clock
import Data.Time

defaultMD :: String
defaultMD = "# Work task tracker\n\n| timestamp | task | duration |\n| --------- | ---- | -------- |"


data Opts = Opts
  { task :: String
  , mins :: Int }

sample :: Parser Opts
sample = Opts
      <$> strOption
          ( long "task"
         <> short 't'
         <> metavar "TARGET"
         <> help "Summary of the task carried out" )
      <*> option auto
          ( long "minutes"
         <> short 'm'
         <> help "How many minutes were spent on the task"
         <> metavar "INT" )

entry :: IO ()
entry = process =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Save a task to a markdown file containing previously carried out tasks"
     <> header "Wot - a command line work tracking tool" )

process :: Opts -> IO ()
process opts = do
    config <- getConfig
    taskProcess config opts

getConfig :: IO Config
getConfig = do
    home <- getHomeDirectory
    let configFilepath = home ++ "/.wot/config"  
    exists <- doesFileExist configFilepath
    if exists 
      then do
        inh <- openFile configFilepath ReadMode
        contents <- hGetContents inh
        if null contents 
          then newConfig configFilepath
          else return Config { filepath = contents }
      else newConfig configFilepath

newConfig :: FilePath -> IO Config
newConfig configFilePath = do
    putStrLn "Please enter the path (from your home directory) to the markdown file you wish to use."
    home <- getHomeDirectory
    markdownFilepath <- getLine
    let absoluteMdFilepath = home ++ "/" ++ markdownFilepath 
    writeFile configFilePath absoluteMdFilepath
    return Config {filepath = absoluteMdFilepath }

data Config where
  Config :: {filepath :: FilePath} -> Config
  deriving Show

taskProcess :: Config -> Opts -> IO ()
taskProcess cfg opts = do
    let fp = filepath cfg
    exists <- doesFileExist fp
    inh <- if exists then openFile fp ReadMode else getDefault fp
    contents <- hGetContents inh
    md <- taskToMarkdown opts
    outh <- openFile "tmp.md" WriteMode
    putStrLn "Saving task to file..."
    hPutStr outh (unlines $ lines contents ++ [md])
    hClose inh
    hClose outh
    removeFile fp
    renameFile "tmp.md" fp
    putStrLn "Task saved to file"

getDefault :: FilePath -> IO Handle
getDefault fp = do
    putStrLn $ "Markdown task list does not exist at " ++ fp ++ ", suggest you check the upstream version - creating default file."
    writeFile fp defaultMD
    openFile fp ReadMode

taskToMarkdown :: Opts -> IO String
taskToMarkdown opts = do
    t <- getCurrentTime
    return $ "| " ++ utcTimeToString t ++ " | " ++ task opts ++ " | " ++ show (mins opts) ++ " |"

utcTimeToString :: UTCTime -> String
utcTimeToString = formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S"