{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HType
    ( app
    )
where

import Model

import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyJPG, loadJuicyPNG)
import Optics.Core (Ixed (ix), headOf, ifiltered, ifindOf, ifolded, itraversed, sumOf, toListOf, traversed, (%&), (%~), (&), (.~), (?~), (^.), (^?), _1, _2)
import Optics.Indexed.Core (imapped, iover, (%))
import Text.Printf (printf)
import Prelude hiding (Word)


app :: IO ()
app = do
    images <- loadImages
    initialModel <- randomModel
    play
        (InWindow "HType" windowSize (10, 10))
        white
        100
        initialModel
        (viewModel images)
        eventHandler
        stepModel


data Images = Images
    { iBackground :: Picture
    , iMine :: Picture
    }


viewModel :: Images -> Model -> Picture
viewModel Images{iBackground, iMine} m =
    mconcat
        [ viewBackground (m ^. mTime) iBackground
        , viewWords m iMine (m ^. mTime)
        , viewTime (m ^. mTime)
        , pausedText (m ^. mPaused)
        ]


viewBackground :: Float -> Picture -> Picture
viewBackground time bgPicture =
    let diff :: Num a => a
        diff = bgImageHeight - windowHeight
        y = fromIntegral ((round $ -5 * time :: Int) `mod` diff) - diff / 2
     in translate 0 y bgPicture


viewWords :: Model -> Picture -> Float -> Picture
viewWords m mine time =
    pictures $
        iover imapped (viewWord mine time (m ^. mFocus) (m ^. mFontHeight)) (m ^. mWords)


viewTime :: Float -> Picture
viewTime seconds =
    printf "%.1f" seconds
        & text
        & color white
        & scaleXY wordScaleFactor
        & translate (windowWidth / 2 - 40) (-windowHeight / 2 + 10)


viewWord :: Picture -> Float -> Maybe Int -> Float -> Int -> Word -> Picture
viewWord mine time focus fontHeight idx (Word x y origWidth str) =
    translate x y $
        pictures
            [ scaleXY wordScaleFactor $
                pictures
                    [ background
                    , translate typedCharsSpace 0 . highlight . text $ fmap fst str
                    ]
            , translate (wordScaleFactor * origWidth) (wordScaleFactor * fontHeight) $
                rotate (90 * time) mine
            ]
  where
    typedCharsSpace = origWidth - sumOf (traversed % _2) str
    background =
        rectangleSolid origWidth fontHeight
            & translate (origWidth / 2) (fontHeight / 4)
            & color (makeColor 0 0 0 0.8)
    highlight = color $ if focus == Just idx then orange else white


eventHandler :: Event -> Model -> Model
eventHandler e m =
    case parseLevelEvent e of
        Just le -> case le of
            TypedLetter c | not (m ^. mPaused) -> case m ^. mFocus of
                Just focIndex
                    | Just ((w, _) : ws) <- m ^? mWords % ix focIndex % wUntypedChars
                    , c == w ->
                        case ws of
                            [] ->
                                m -- no letters left - remove the word and unfocus it
                                    & mWords %~ deleteAt focIndex
                                    & mFocus .~ Nothing
                            _ ->
                                m -- First letter of focused word typed -> remove it
                                    & mWords % ix focIndex % wUntypedChars .~ ws
                -- Nothing focused and there's a word starting with the pressed letter,
                -- remove the letter an focus that word
                Nothing -> case ifindOf (mWords % itraversed) (\_ w -> headOf (wUntypedChars % traversed % _1) w == Just c) m of
                    Just (focIndex, Word _ _ _ str) -> case str of
                        [_] ->
                            m -- one-letter word -> just remove it without focusing anything
                                & mWords %~ deleteAt focIndex
                        _ ->
                            m -- more letters -> remove typed letter and focus it
                                & mWords % ix focIndex % wUntypedChars %~ tail
                                & mFocus ?~ focIndex
                    Nothing -> m
                _ -> m
            TogglePause -> m & mPaused %~ not
            _ -> m
        Nothing -> m


data LevelEvent
    = TypedLetter Char
    | TogglePause


parseLevelEvent :: Event -> Maybe LevelEvent
parseLevelEvent e = case e of
    EventKey (Char c) Down _ _ -> Just (TypedLetter c)
    EventKey (SpecialKey KeySpace) Down _ _ -> Just TogglePause
    _ -> Nothing


deleteAt :: Int -> [a] -> [a]
deleteAt idx =
    toListOf (ifolded %& ifiltered (\i _ -> i /= idx))


scaleXY :: Float -> Picture -> Picture
scaleXY s = scale s s


stepModel :: Float -> Model -> Model
stepModel dt m
    | m ^. mPaused = m
    | otherwise =
        m
            & mTime %~ (+ dt)
            & mWords % traversed % wy %~ subtract (10 * dt)


loadImages :: IO Images
loadImages = do
    iBackground <- fromMaybe Blank <$> loadJuicyJPG "images/stars.jpg"
    iMine <- fromMaybe Blank <$> loadJuicyPNG "images/mine.png"
    pure Images{..}

-- TODO make the words move towards the gun
-- TODO add sound feedback on mistyped letter / destroyed word
-- TODO generate different levels
--    Lower levels have fewer words and the words are generally shorter, slower-flying words
-- TODO add state transitions between START - LEVEL 1+ - GAME OVER screens
-- TODO draw ship, turn it as it fires
-- TODO add different types of enemies
