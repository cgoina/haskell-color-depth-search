module Args (
    CDSArgs
  , parseCmdArgs
  , masksPaths
) where

import Options.Applicative
import Data.Semigroup ((<>))

data CDSArgs = CDSArgs { 
    masksPaths :: FilePath
  , dataPaths :: FilePath
  , maskThreshold :: Float
  , dataThreshold :: Float
} deriving (Show)

cdsArgs :: Parser CDSArgs
cdsArgs = CDSArgs 
   <$> strOption
       ( long "masks"
       <> help "Image Masks" )
   <*> strOption
       ( long "images"
       <> help "Searched Image Inputs" )
   <*> option auto
       ( long "maskThreshold"
       <> value 0.01
       <> help "Mask threshold" )
   <*> option auto
       ( long "dataThreshold"
       <> value 0.01
       <> help "Mask threshold" )

parseCmdArgs :: IO CDSArgs
parseCmdArgs = execParser cdsArgsInfo
    where
        cdsArgsInfo = info
            (helper <*> versionOption <*> cdsArgs)
            (fullDesc 
                <> progDesc "Find color depth search matches" 
                <> header
                 "find CDS matches between input images and the given masks")
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.1" (
                          long "version" 
                       <> help "Show version")
