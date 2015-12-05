module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import qualified Web.Scotty as Scotty

import Prime (prime)

data Option = Option
    { port :: Int
    }
  deriving (Show)

optAuto :: (Read a, Show a) => String -> String -> String -> a -> Parser a
optAuto name metavar help val = Opt.option Opt.auto
    (Opt.long name <> Opt.metavar metavar <> Opt.help help <> Opt.showDefault <> Opt.value val)

optParser :: Parser Option
optParser = Option
    <$> optAuto "port" "PORT" "port number" 50001

parserInfo :: ParserInfo Option
parserInfo = Opt.info (Opt.helper <*> optParser) Opt.fullDesc

main :: IO ()
main = do
    opt <- Opt.execParser parserInfo
    hSetBuffering stdout NoBuffering
    Scotty.scotty (port opt) $ do
        Scotty.middleware logStdout
        Scotty.get "/" $ do
            n <- Scotty.param "n" `Scotty.rescue` \_ -> return 12
            if 1 <= n && n <= 15
                then do
                    r <- liftIO $ prime n
                    Scotty.text $ Text.pack $ show r
                else Scotty.text "error: 1 <= n <= 15"
