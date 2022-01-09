{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Model where

import Control.Monad (filterM)
import Data.Char (isAsciiLower, toLower)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Graphics.Gloss (
    Picture (Blank),
    color,
    text,
    translate,
    white,
 )
import Graphics.UI.GLUT.Fonts (
    StrokeFont (Roman),
    fontHeight,
    stringWidth,
 )
import Optics.Core (
    sumOf,
    traversed,
    (%),
    (&),
    _2,
 )
import Optics.TH (makeLenses)
import System.Random.Stateful (
    Uniform (uniformM),
    UniformRange (uniformRM),
    newIOGenM,
    newStdGen,
 )
import Prelude hiding (Word)

-- Using GLUT to query string width and font height based on
-- https://stackoverflow.com/questions/59337172/how-do-find-the-width-of-a-text-picture-in-gloss#answer-59337636
import qualified Graphics.UI.GLUT.Initialization as GLUT


data Model = Model
    { _mWords :: [Word]
    , _mFocus :: Maybe Int
    , _mFontHeight :: Float
    , _mTime :: Float
    , _mPaused :: Bool
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
    let maxX = windowWidth / 2
    pickedWords <-
        traverse
            ( \wordsWithSameLetter -> do
                w <- (wordsWithSameLetter !!) <$> uniformRM (0, length wordsWithSameLetter - 1) g
                charsWithWidth <- traverse (\c -> (c,) . fromIntegral <$> stringWidth Roman [c]) w
                let wordWidth = sumOf (traversed % _2) charsWithWidth
                x <- uniformRM (- maxX, maxX - wordScaleFactor * wordWidth) g
                y <- (\y' -> windowHeight * (0.5 + 0.1 * y')) <$> uniformRM (0, 1) g
                pure (Word x y wordWidth charsWithWidth)
            )
            listsToKeep
    _mFontHeight <- fontHeight Roman
    GLUT.exit
    pure $ Model pickedWords Nothing _mFontHeight 0 False


simplifyWord :: String -> Maybe String
simplifyWord word
    | all isAsciiLower word = Just word
    | otherwise = Nothing


windowSize :: (Int, Int)
windowSize = (windowWidth, windowHeight)


windowWidth, windowHeight, bgImageHeight :: Num a => a
windowWidth = 480
windowHeight = 720
bgImageHeight = 1138


playerX, playerY :: Fractional a => a
playerX = 0
playerY = (-0.45) * windowHeight


pausedText :: Bool -> Picture
pausedText False = Blank
pausedText True =
    text "Paused"
        & color white
        & translate -212.5 {-half of `print =<< stringWidth Roman "Paused"` -} 0


wordScaleFactor :: Float
wordScaleFactor = 0.15
