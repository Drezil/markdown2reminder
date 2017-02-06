module Lib
    ( getMails
    , Mail
    ) where

import Text.Pandoc
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Definition
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Monoid
import Data.Maybe
import Debug.Trace

data Mail = Mail
          { title   ::  String
          , content ::  String
          , to      ::  Maybe String --Alternate Adress to send to
          } deriving (Eq, Show)

mkTestmail :: (Show a) => a -> (Day,Mail)
mkTestmail s = (fromGregorian 1999 1 1, Mail "Test" (show s) Nothing)

getMails :: String -> Either PandocError [(Day, Mail)]
getMails markdown = do
        (Pandoc meta document) <- readMarkdown def markdown
        return $ filter isBullet document >>= mkMail


isBullet :: Block -> Bool
isBullet (BulletList _) = True
isBullet _              = False

mkMail :: Block -> [(Day, Mail)]
mkMail (BulletList blocks) = catMaybes $ blToMail <$> blocks
mkMail _ = []

blToMail :: [Block] -> Maybe (Day, Mail)
blToMail (Para (Str dat:Space:tit):CodeBlock ca cont:_) = trace (show ca) dayMailPair
        where
                dayofmail d = parseTimeM True defaultTimeLocale "%d.%m." d :: Maybe Day
                titleofmail = writePlain def (Pandoc (Meta mempty) [Para tit])
                contentofmail (_,a,_) = if "mail" `elem` a then Just cont else Nothing
                completeMail :: Maybe Mail
                completeMail = Mail <$> pure titleofmail
                                    <*> contentofmail ca
                                    <*> pure Nothing
                dayMailPair :: Maybe (Day, Mail)
                dayMailPair = (,) <$> dayofmail dat
                                  <*> completeMail
blToMail _ = Nothing -- Just $ mkTestmail a
