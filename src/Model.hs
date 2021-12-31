{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Model where

import Control.Monad (filterM)
import Data.Char (isAsciiLower, toLower)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Graphics.UI.GLUT.Fonts (StrokeFont (Roman), fontHeight, stringWidth)
import Optics.Core (sumOf, traversed, (%), _2)
import Optics.TH (makeLenses)
import System.Random.Stateful (Uniform (uniformM), UniformRange (uniformRM), newIOGenM, newStdGen)
import Prelude hiding (Word)

-- Using GLUT to query string width and font height based on
-- https://stackoverflow.com/questions/59337172/how-do-find-the-width-of-a-text-picture-in-gloss#answer-59337636
import qualified Graphics.UI.GLUT.Initialization as GLUT


data Model = Model
    { _mWords :: [Word]
    , _mFocus :: Maybe Int
    , _mFontHeight :: Float
    }
    deriving (Show)


data Word = Word
    { _wx :: Float
    , _wy :: Float
    , _wOrigWidth :: Float
    , _wUntypedChars :: [(Char, Float)]
    }
    deriving (Show)


makeLenses ''Model
makeLenses ''Word


randomModel :: IO Model
randomModel = do
    _ <- GLUT.getArgsAndInitialize
    ws <- mapMaybe simplifyWord . words . fmap toLower <$> readFile "text"
    stdGen <- newStdGen
    g <- newIOGenM stdGen
    let byFirstLetter = groupBy ((==) `on` head) $ sort ws
    listsToKeep <- filterM (\_ -> uniformM g) byFirstLetter
    let maxX = fst windowSize `div` 2
        maxY = snd windowSize `div` 2
    pickedWords <-
        traverse
            ( \wordsWithSameLetter -> do
                w <- (wordsWithSameLetter !!) <$> uniformRM (0, length wordsWithSameLetter - 1) g
                charsWithWidth <- traverse (\c -> (c,) . fromIntegral <$> stringWidth Roman [c]) w
                let wordWidth = sumOf (traversed % _2) charsWithWidth
                x <- fromIntegral <$> uniformRM (- maxX, maxX - round (wordScaleFactor * wordWidth)) g
                y <- fromIntegral <$> uniformRM (- maxY, maxY - 30) g
                pure (Word x y wordWidth charsWithWidth)
            )
            listsToKeep
    _mFontHeight <- fontHeight Roman
    GLUT.exit
    pure $ Model pickedWords Nothing _mFontHeight


simplifyWord :: String -> Maybe String
simplifyWord word
    | all isAsciiLower word = Just word
    | otherwise = Nothing


windowSize :: (Int, Int)
windowSize = (480, 720)


wordScaleFactor :: Float
wordScaleFactor = 0.15
