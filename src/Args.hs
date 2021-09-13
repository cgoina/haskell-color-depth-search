module Args (
    CDSArgs
  , parseCmdArgs
  , masksPaths
  , dataPaths
  , noMaskMirroring
  , maskThreshold
  , dataThreshold
  , maxFilterRadius
  , pixColorFluctuation
) where

import Options.Applicative
    ( auto,
      fullDesc,
      header,
      help,
      info,
      infoOption,
      long,
      option,
      progDesc,
      strOption,
      value,
      execParser,
      helper,
      Parser, switch )
import Data.Semigroup ((<>))


data CDSArgs = CDSArgs {
    masksPaths :: FilePath
  , dataPaths :: FilePath
  , maxFilterRadius :: Float
  , noMaskMirroring :: !Bool
  , maskThreshold :: Double
  , dataThreshold :: Double
  , pixColorFluctuation :: Double
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
       ( long "maxFilterRadius"
       <> value 5
       <> help "Mask threshold" )
   <*> switch 
       ( long "noMaskMirroring"
       <> help "If set there's no mask mirroring")
   <*> option auto
       ( long "maskThreshold"
       <> value 0.01
       <> help "Mask threshold" )
   <*> option auto
       ( long "dataThreshold"
       <> value 0.01
       <> help "Mask threshold" )
   <*> option auto
        ( long "pixColorFluctuation"
        <> value 2.0
        <> help "Pixel color fluctuation which is equivalent to z fluctuation")


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

