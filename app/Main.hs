{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Ledger hiding (singleton)
import Options.Applicative

import AnimAliens

data Opts = Opts
  { team :: PubKeyHash
  , project :: PubKeyHash
  , community :: PubKeyHash
  , policyId :: CurrencySymbol
  , output :: FilePath
  } deriving Show

main :: IO ()
main = createSC =<< execParser opts

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) . mconcat $
  [ fullDesc
  , progDesc "Create a smart contract"
  ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (strOption . mconcat $
    [ long "team"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "project"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "community"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "policy-id"
    , metavar "PID"
    ])
  <*> (strOption . mconcat $
    [ long "output"
    , metavar "FILE"
    , help "Where to write the script."
    ])

createSC :: Opts -> IO ()
createSC Opts{..} = do
  result <- writeFileTextEnvelope output Nothing . tradeSerialised $ newContractInfo policyId team project community
  case result of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ output
