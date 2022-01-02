{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module HType (
    app,
) where

import Model

import Graphics.Gloss.Interface.Pure.Game
import Optics.Core (Ixed (ix), headOf, ifiltered, ifindOf, ifolded, itraversed, sumOf, toListOf, traversed, (%&), (%~), (&), (.~), (?~), (^.), (^?), _1, _2)
import Optics.Indexed.Core (imapped, iover, (%))
import Text.Printf (printf)
import Prelude hiding (Word)


app :: IO ()
app = do
    initialModel <- randomModel
    play
        (InWindow "HType" windowSize (10, 10))
        white
        10
        initialModel
        viewModel
        eventHandler
        stepModel


viewModel :: Model -> Picture
viewModel m =
    mconcat
        [ viewWords m
        , viewTime (m ^. mTime)
        , pausedText (m ^. mPaused)
        ]


viewWords :: Model -> Picture
viewWords m =
    pictures $
        iover imapped (viewWord (m ^. mFocus) (m ^. mFontHeight)) (m ^. mWords)


viewTime :: Float -> Picture
viewTime seconds =
    printf "%.1f" seconds
        & text
        & scaleXY wordScaleFactor
        & translate 0 (- windowHeight / 2 + 10)


viewWord :: Maybe Int -> Float -> Int -> Word -> Picture
viewWord focus fontHeight idx (Word x y origWidth str) =
    translate x y . scaleXY wordScaleFactor $
        pictures
            [ background
            , translate typedCharsSpace 0 . highlight . text $ fmap fst str
            ]
  where
    typedCharsSpace = origWidth - sumOf (traversed % _2) str
    background =
        rectangleSolid origWidth fontHeight
            & translate (origWidth / 2) (fontHeight / 4)
            & color (greyN 0.9)
    highlight = if focus == Just idx then color red else id


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
    | otherwise = m & mTime %~ (+ dt)

-- TODO make the words move towards the gun
-- TODO add sound feedback on mistyped letter / destroyed word
-- TODO generate different levels
--    Lower levels have fewer words and the words are generally shorter, slower-flying words
-- TODO add state transitions between START - LEVEL 1+ - GAME OVER screens
