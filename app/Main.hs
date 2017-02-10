module Main where

import Lib
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import Data.Monoid
import Network.Mail.Mime hiding (mailFrom, mailTo)
import Options.Applicative

data CLIOptions = CLIOptions
        { filename     ::  String
        , mailTo       ::  String
        , mailFrom     ::  String
        , mailFromName ::  String
        }

options :: Parser CLIOptions
options = CLIOptions
        <$> strOption
                ( long "filename"
                <> short 'f'
                <> metavar "FILE"
                <> help "Filename of Markdown-File"
                )
        <*> strOption
                ( long "to"
                <> short 't'
                <> metavar "TO"
                <> help "Mail-address to send the reminder to"
                )
        <*> strOption
                ( long "from"
                <> short 'f'
                <> metavar "FROM"
                <> help "Mail-address of the reminder"
                )
        <*> strOption
                ( long "name"
                <> short 'n'
                <> metavar "NAME"
                <> help "Name in the reminder-mails"
                )

opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Send reminder from FILE to mail TO using FROM and NAME as identification for the sender"
        <> header "md2mail - a small program for sending out reminder-mails from markdown"
        )

main :: IO ()
main = do
        args <- execParser opts
        md <- readFile $ filename args
        (UTCTime today _) <- getCurrentTime
        sequence_ $ sequence . fmap (renderSendMail . snd) . filter (filterToday today) <$> getMails md (mailTo args) (mailFrom args) (mailFromName args)


filterToday :: Day -> (Day, Mail) -> Bool
filterToday d (d2,_) = day1 == day2 && m1 == m2
        where
                (_,m1,day1) = toGregorian d
                (_,m2,day2) = toGregorian d2
