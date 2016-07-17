module Main where

import Options.Applicative
import System.IO (hPutStrLn, stderr)
import City

data Args = Args
    {   width :: Int
      , height :: Int
      , depth :: Int 
      , mesh :: Bool }

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

meshParser :: Parser Bool
meshParser = switch
              (  long "mesh"
              <> short 'm'
              <> help "Whether or not to print only the meshes" )
 
args :: Parser Args
args = Args
    <$> widthParser
    <*> heightParser
    <*> depthParser
    <*> meshParser

printCity :: Args -> IO ()
printCity (Args w h d m) = do
    city <- genCity (0, 0) (w, h) d
    putStrLn $ show w ++ " " ++ show h
    if m
        then putStrLn $ cityMeshToStr city
        else putStrLn $ cityToStr city

    -- For debugging only! Prints the cityToStr result
    -- to stderr
    hPutStrLn stderr $ cityToStr city

main :: IO ()
main = execParser opts >>= printCity
    where
        opts = info args
            (  fullDesc 
            <> progDesc "Generate a city of WIDTH*HEIGHT with a depth of DEPTH"
            <> header "procedural-city - Generate a procedural city" )
