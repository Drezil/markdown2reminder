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
import Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- data Mail = Mail
--           { title   ::  String
--           , content ::  String
--           , to      ::  Maybe String --Alternate Adress to send to
--           } deriving (Eq, Show)

-- mkTestmail :: (Show a) => a -> (Day,Mail)
-- mkTestmail s = (fromGregorian 1999 1 1, Mail "Test" (show s) Nothing)

getMails :: String -> String -> String -> String -> Either PandocError [(Day, Mail)]
getMails markdown to from name= do
        (Pandoc meta document) <- readMarkdown def markdown
        return $ filter isBullet document >>= mkMail to from name


isBullet :: Block -> Bool
isBullet (BulletList _) = True
isBullet _              = False

mkMail :: String -> String -> String -> Block -> [(Day, Mail)]
mkMail to from name (BulletList blocks) = catMaybes $ blToMail to from name <$> blocks
mkMail _ _ _ _ = []

blToMail :: String -> String -> String -> [Block] -> Maybe (Day, Mail)
blToMail to from name (Para (Str dat:Space:tit):CodeBlock ca cont:_) = dayMailPair
        where
                dayofmail d = parseTimeM True defaultTimeLocale "%d.%m." d :: Maybe Day
                titleofmail = writePlain def (Pandoc (Meta mempty) [Para tit])
                contentofmail (_,a,_) = if "mail" `elem` a then Just cont else Nothing
                completeMail :: Maybe Mail
                completeMail = simpleMail' <$> pure (Address (Just "Fachschaft Technik") (T.pack to)) --To
                                           <*> pure (Address (Just (T.pack name)) (T.pack from)) --From
                                           <*> pure (T.pack titleofmail)
                                           <*> (TL.pack <$> contentofmail ca)
                dayMailPair :: Maybe (Day, Mail)
                dayMailPair = (,) <$> dayofmail dat
                                  <*> completeMail
blToMail _ _ _ _ = Nothing -- Just $ mkTestmail a
