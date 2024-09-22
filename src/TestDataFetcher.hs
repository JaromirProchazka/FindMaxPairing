{-# LANGUAGE DeriveGeneric #-}

module TestDataFetcher where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), (.:), (.=), encode, decode, eitherDecode)
import Data.Aeson.Key (Key, fromString)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO (readFile, writeFile)
import Control.Monad (forM_)
import System.Directory.ProjectRoot (getProjectRoot)
import System.FilePath ((</>))
import GHC.Generics (Generic)
import Control.Applicative (liftA2)
import System.IO.Error (tryIOError)
import qualified Data.Text as T
import Debug.Trace (trace)

import System.Directory (getCurrentDirectory)
import System.FilePath
import qualified Data.List as List
import qualified System.FilePath as FilePath


data Partites = Partites [(String, [String])] deriving (Show, Generic)

instance ToJSON Partites where
    toJSON (Partites pairs) = Aeson.object [ (fromString "graph") .= pairs ]

instance FromJSON Partites where
    parseJSON (Object v) = Partites <$> v .: (fromString "graph")
    parseJSON _ = fail "Expected an object"

partitesToType :: IO Partites -> IO [(String, [String])]
partitesToType ioPartites = do
    Partites pairs <- ioPartites
    pure pairs

-- Function to decode JSON to data
decodeFromJSON :: B.ByteString -> Either String Partites
decodeFromJSON d = eitherDecode d

-- Function to read data from file
-- EXAMPLE:
-- maybeData <- readDataFromFile "output.json"
-- case maybeData of
--         Just dataStructure -> print dataStructure
--         Nothing            -> putStrLn "Failed to parse JSON"
readDataFromFile :: FilePath -> IO Partites
readDataFromFile fileRelativeName = do
    c <- getCurrentDirectory
    let cc = trace ("CD: " ++ (show c)) c
    let filePath = cc </> "tests" </> "TestsData" </> fileRelativeName
    content <- B.readFile (trace ("The test data file path: " ++ show filePath) filePath)
    case decodeFromJSON (trace ("Contents: " ++ (show content)) content) of
        Right decodedData -> return decodedData
        Left error -> do
            putStrLn $ "Failed to parse JSON: " ++ show error
            return $ Partites []