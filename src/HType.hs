{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module HType (
    app,
) where

import Model

import Data.Text (Text)
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game
import Optics.Core (Ixed (ix), folded, ifiltered, ifindOf, ifolded, itraversed, toListOf, (%&), (%~), (&), (.~), (?~), (^.), (^..), (^?))
import Optics.Indexed.Core (imapped, iover, (%))

import Prelude hiding (Word)


app :: IO ()
app = do
    initialModel <- randomModel
    print initialModel
    play
        (InWindow "HType" windowSize (10, 10))
        white
        10
        initialModel
        viewModel
        eventHandler
        stepModel


initModel :: Model
initModel =
    Model
        { _mWords =
            [ Word -350 250 "a"
            , Word 0 250 "hello"
            , Word 350 250 "world"
            ]
        , _mFocus = Nothing
        }


viewModel :: Model -> Picture
viewModel m =
    (m ^. mWords)
        & iover imapped (viewWord (m ^. mFocus))
        & pictures


viewWord :: Maybe Int -> Int -> Word -> Picture
viewWord mFocus idx (Word x y str) =
    translate x y . scaleXY 0.15 . highlight $ text str
  where
    highlight = if mFocus == Just idx then color red else id


eventHandler :: Event -> Model -> Model
eventHandler e m
    -- First letter of focused word typed -> remove it
    | EventKey (Char c) Down _ _ <- e
      , Just focIndex <- m ^. mFocus
      , Just (Word _ _ (w : ws)) <- m ^? mWords % ix focIndex
      , c == w =
        case ws of
            [] ->
                m -- no letters left - remove the word and unfocus it
                    & mWords %~ deleteAt focIndex
                    & mFocus .~ Nothing
            _ ->
                m
                    & mWords % ix focIndex % wChars .~ ws
    -- Nothing focused and there's a word starting with the pressed letter,
    -- remove the letter an focus that word
    | EventKey (Char c) Down _ _ <- e
      , Nothing <- m ^. mFocus =
        case ifindOf (mWords % itraversed) (\_ (Word _ _ (x : _)) -> x == c) m of
            Just (focIndex, Word _ _ str) -> case str of
                [_] ->
                    m -- one-letter word -> just remove it without focusing anything
                        & mWords %~ deleteAt focIndex
                _ ->
                    m -- more letters -> remove typed letter and focus it
                        & mWords % ix focIndex % wChars %~ tail
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
