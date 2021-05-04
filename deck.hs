import Data.Bits
import Data.List
import Data.Maybe
import Control.Monad
import System.Random
import System.Random.Shuffle


colors = ["Karo", "Herz", "Pik", "Kreuz"]
symbols = ["Ass", "Koenig", "Dame", "Bube", "10", "9", "8", "7"]
deckSize = (length colors) * (length symbols)


data Game = Game { deck :: [Int]
                 , hands :: [[Int]]
                 , players :: [Int]
                 , activePlayer :: Int
                 } deriving (Show)


-- TODO Deckgröße abhängig von Handgröße und Spielerzahl, MauMau-Regeln

main :: IO()
main = do 
  putStrLn "Welcome to MauMau!"
  putStrLn " "
  putStrLn "How many players?"
  num_players <- getLine
  putStrLn " "
  putStrLn "How many cards per hand?"
  num_hand <- getLine
  putStrLn " "
  putStrLn ("Generating deck for " ++ (show num_players) ++ " players")
  putStrLn "Shuffling deck"
  g <- getStdGen
  ps <- return ([0..((read num_players)-1)])
  game <- return (Game (shuffle' buildDeck deckSize g) [[]] ps 0)
  mapM putStrLn (decode (deck game))
  putStrLn " "
  putStrLn "Preparing players hands"
  game <- return (Game (reduceDeck (read num_players) (read num_hand) (deck game)) (takeHand (read num_players) (read num_hand) (deck game) (hands game)) (players game) 0)
  putStrLn " "
  putStrLn " "
  putStrLn "Starting game with 2 players:"
  turn 0 game


-- Make turns, take hands and cards, game loop in "turn"
turn :: Int -> Game -> IO ()
turn p game = do
  putStrLn ("Turn of player number " ++ show p)
  game <- return (takeCard p game)
  if length (deck game) == 0 then putStrLn ("Game over, winning player is " ++ show p)
  else turn (nextPlayer p game) game


printHands :: Int -> Game -> IO ()
printHands n game = do
  mapM putStrLn (decode ((hands game) !! (n-1)))
  putStrLn " "
  when ((n-1) > 0) $ printHands (n-1) game


takeCard :: Int -> Game -> Game
takeCard p game = Game (reduceDeck 1 1 (deck game)) (takeFrom p game) (players game) p


takeHand :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
takeHand p n deck hands = [ take n (drop hand_length deck) ++ [] | hand_length <- [0,n..((p-1)*n)]]





takeFrom :: Int -> Game -> [[Int]]
takeFrom p game = [ if fromJust (elemIndex hand (hands game)) == p then head (take 1 (deck game)) : hand else hand | hand <- (hands game)]



reduceDeck :: Int -> Int -> [Int] -> [Int]
reduceDeck p n deck = drop (p * n) deck 



nextPlayer :: Int -> Game -> Int
nextPlayer p game
  | p == lastPlayer = 0
  | otherwise       = p + 1
  where
    lastPlayer = (length (players game)) - 1





-- Deck creation and string representation
buildDeck :: [Int]
buildDeck = [ encode "color" color .|. encode "symbol" symbol | color <- [0..((length colors) - 1)], symbol <- [0..((length symbols) - 1)]]



decode :: [Int] -> [String]
decode deck = [ decode' x | x <- deck]



-- Decoding
decode' :: Int -> String
decode' n = colors!!a ++ " " ++ symbols!!b
  where
    a = shiftR (n .&. 49152) 14   -- bit 15,16 to 1
    b = findSetBit (n .&. 16383)  -- first 14 bits to 1

findSetBit :: Int -> Int
findSetBit n = fromJust (elemIndex n (1 : (take 13 (powers 2))))

powers :: Int -> [Int]
powers n = n : map (* n) (powers n)


-- Encoding
encode :: String -> Int -> Int
encode t v
  | t == "color"  = encodeColor v
  | t == "symbol" = encodeSymbol v

encodeColor :: Int -> Int
encodeColor n = shift n 14

encodeSymbol :: Int -> Int
encodeSymbol n = shift 1 n
