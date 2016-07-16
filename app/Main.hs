module Main where

import Options.Applicative
import City

data Args = Args
    {   width :: Int
      , height :: Int
      , depth :: Int }

widthParser :: Parser Int
widthParser = option auto
              (  long "width"
              <> short 'w'
              <> metavar "WIDTH"
              <> help "Width of city" )

heightParser :: Parser Int
heightParser = option auto
              (  long "height"
              <> short 'h'
              <> metavar "HEIGHT"
              <> help "Height of city" )

depthParser :: Parser Int
depthParser = option auto
              (  long "depth"
              <> short 'd'
              <> metavar "DEPTH"
              <> help "Number of times to iterate" )
 
args :: Parser Args
args = Args
    <$> widthParser
    <*> heightParser
    <*> depthParser

printCity :: Args -> IO ()
printCity (Args w h d) = do
    putStr $ show w
    putStr " "
    putStr $ show h
    putStrLn ""
    cityToStr <$> genCity (0, 0) (w, h) d >>= putStrLn

main :: IO ()
main = execParser opts >>= printCity
    where
        opts = info args
            (  fullDesc 
            <> progDesc "Generate a city of WIDTH*HEIGHT with a depth of DEPTH"
            <> header "procedural-city - Generate a procedural city" )
