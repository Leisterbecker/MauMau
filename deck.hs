import Data.Bits
import Data.List
import Data.Maybe


colors = ["Karo", "Herz", "Pik", "Kreuz"]
symbols = ["Joker", "Ass", "Koenig", "Dame", "Bube", "10", "9", "8", "7", "6", "5", "4", "3", "2"]
deck = decodeDeck

printDeck = do { print " "
               ; print "Kartendeck:"
               ; print " "
               ; mapM print (shuffle deck 378) --(shuffle deck)
               ; print " " }



-- shuffle
shuffle :: [String] -> Int -> [String] -- tar im then teil muss bereits eine liste sein
shuffle tar n = if n > 0 then (shuffle (innerShuffle (split tar)) (n-1)) else tar

split :: [String] -> ([String],[String])
split ar = splitList ar mid
  where
    mid = (length deck) `div` 2

innerShuffle :: ([String],[String]) -> [String]
innerShuffle s = takeAll (fst s) (snd s)
  where
    takeAll :: [String] -> [String] -> [String]
    takeAll a b = if a == [] then [] else head a : if b == [] then [] else head b : takeAll (tail a) (tail b)

splitList :: [String] -> Int -> ([String],[String])
splitList xs n = (take n xs , drop n xs )



-- Main functions
decodeDeck :: [String]
decodeDeck = [ decode x | x <- buildDeck]

buildDeck :: [Int]
buildDeck = [ encode "color" color .|. encode "symbol" symbol | color <- [0..((length colors) - 1)], symbol <- [0..((length symbols) - 1)]]



-- Decoding
decode :: Int -> String
decode n = colors!!a ++ " " ++ symbols!!b
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
