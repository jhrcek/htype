{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module HType (
    app,
) where

import Model

import Data.Text (Text)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Interface.Pure.Game
import Optics.Core (Ixed (ix), folded, ifiltered, ifindOf, ifolded, itraversed, sumOf, toListOf, traversed, (%&), (%~), (&), (.~), (?~), (^.), (^..), (^?), _2)
import Optics.Indexed.Core (imapped, iover, (%))
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
    (m ^. mWords)
        & iover imapped (viewWord (m ^. mFocus) (m ^. mFontHeight))
        & pictures


viewWord :: Maybe Int -> Float -> Int -> Word -> Picture
viewWord mFocus fontHeight idx (Word x y origWidth str) =
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
    highlight = if mFocus == Just idx then color red else id


eventHandler :: Event -> Model -> Model
eventHandler e m
    -- First letter of focused word typed -> remove it
    | EventKey (Char c) Down _ _ <- e
      , Just focIndex <- m ^. mFocus
      , Just (Word _ _ _ ((w, _) : ws)) <- m ^? mWords % ix focIndex
      , c == w =
        case ws of
            [] ->
                m -- no letters left - remove the word and unfocus it
                    & mWords %~ deleteAt focIndex
                    & mFocus .~ Nothing
            _ ->
                m
                    & mWords % ix focIndex % wUntypedChars .~ ws
    -- Nothing focused and there's a word starting with the pressed letter,
    -- remove the letter an focus that word
    | EventKey (Char c) Down _ _ <- e
      , Nothing <- m ^. mFocus =
        case ifindOf (mWords % itraversed) (\_ (Word _ _ _ ((x, _) : _)) -> x == c) m of
            Just (focIndex, Word _ _ _ str) -> case str of
                [_] ->
                    m -- one-letter word -> just remove it without focusing anything
                        & mWords %~ deleteAt focIndex
                _ ->
                    m -- more letters -> remove typed letter and focus it
                        & mWords % ix focIndex % wUntypedChars %~ tail
                        & mFocus ?~ focIndex
            Nothing -> m
    | otherwise = m


deleteAt :: Int -> [a] -> [a]
deleteAt idx =
    toListOf (ifolded %& ifiltered (\i _ -> i /= idx))


scaleXY :: Float -> Picture -> Picture
scaleXY s = scale s s


stepModel :: Float -> Model -> Model
stepModel dt m = m

-- TODO make the words move towards the gun
-- TODO add sound feedback on mistyped letter / destroyed word
