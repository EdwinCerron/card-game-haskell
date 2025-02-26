{-# LANGUAGE RecordWildCards #-}

module Main where

import Game.Blackjack
import Game.Card.Standard
import Game.Card.Types (getFace, getSuit)
import System.Random ()
import Game.Card.Deck (shuffle)
import Control.Monad ()

showCard :: StandardCard -> String
showCard card = show (getFace card) ++ " of " ++ show (getSuit card)

showHand :: [StandardCard] -> String
showHand = unlines . map showCard

showPlayer :: String -> Player -> String
showPlayer name Player{..} = unlines
    [ name ++ ":"
    , showHand playerHand
    , "Puntuación: " ++ show playerScore
    ]

showGame :: BlackjackGame -> IO ()
showGame BlackjackGame{..} = do
    putStrLn "\n=== Estado Actual del Juego ==="
    putStr $ showPlayer "Jugador" gamePlayer
    putStrLn "Dealer:"
    case gameState of
        GameOver -> putStr $ showPlayer "Dealer" gameDealer
        _ -> case playerHand gameDealer of
            [] -> putStrLn "Error: Mano del dealer vacía"
            (firstCard:_) -> putStrLn $ showCard firstCard ++ "\n[Carta oculta]"
    putStrLn "================================\n"

gameLoop :: BlackjackGame -> IO ()
gameLoop game = do
    showGame game
    if isGameOver game
        then showFinalResult game
        else do
            action <- getPlayerAction
            case action of
                'h' -> gameLoop $ hit game
                's' -> gameLoop $ stand game
                _   -> gameLoop game

getPlayerAction :: IO Char
getPlayerAction = do
    putStrLn "¿Qué deseas hacer?"
    putStrLn "(h) Pedir carta"
    putStrLn "(s) Plantarse"
    getChar

showFinalResult :: BlackjackGame -> IO ()
showFinalResult game@BlackjackGame{..} = do
    putStrLn "\n=== Fin del Juego ==="
    showGame game
    case getWinner game of
        Just player
            | player == gamePlayer -> putStrLn "¡Felicidades! ¡Has ganado!"
            | otherwise -> putStrLn "El dealer gana. Mejor suerte la próxima vez."
        Nothing -> putStrLn "¡Empate!"

main :: IO ()
main = do
    putStrLn "¡Bienvenido a Blackjack!"
    putStrLn "Repartiendo cartas..."
    shuffledDeck <- shuffle newStandardDeck
    let game = newGame shuffledDeck
    gameLoop game
