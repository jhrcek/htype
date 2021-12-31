{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Monad
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Optics.TH (makeLenses)
import System.Random.Stateful
import Prelude hiding (Word)


data Model = Model
    { _mWords :: [Word]
    , _mFocus :: Maybe Int
    }
    deriving (Show)


data Word = Word
    { _wx :: Float
    , _wy :: Float
    , _wChars :: String
    }
    deriving (Show)


makeLenses ''Model
makeLenses ''Word


randomModel :: IO Model
randomModel = do
    ws <- mapMaybe acceptableWord . words <$> readFile "text"
    stdGen <- newStdGen
    g <- newIOGenM stdGen
    let byFirstLetter = groupBy ((==) `on` head) $ sort ws
    listsToKeep <- filterM (\_ -> uniformM g) byFirstLetter
    let maxX = fst windowSize `div` 2 - 50
        maxY = snd windowSize `div` 2 - 50
    pickedWords <-
        mapM
            ( \wordsWithSameLetter -> do
                x <- fromIntegral <$> uniformRM (- maxX, maxX) g
                y <- fromIntegral <$> uniformRM (- maxY, maxY) g
                w <- (wordsWithSameLetter !!) <$> uniformRM (0, length wordsWithSameLetter - 1) g
                pure (Word x y w)
            )
            listsToKeep
    pure $ Model pickedWords Nothing


acceptableChar :: Char -> Bool
acceptableChar c =
    isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ",.?!()-\'\""


acceptableWord :: String -> Maybe String
acceptableWord w = case filter acceptableChar w of
    [] -> Nothing
    w' -> Just w'


windowSize :: (Int, Int)
windowSize = (800, 600)