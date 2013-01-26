import Control.Monad
import Color
import Data.List (intersect, sort, intersperse, partition)
import Data.Ratio
import Control.Exception

balls = [1..90]

ncr n k | k < 0 || k > n = 0
        | otherwise      = product (enumFromTo (k+1) n) `div` product (enumFromTo 1 (n-k))

winningAfter n k r | r < k = 0
                   | r > n = ncr n k
		   | otherwise = (ncr r k)

winningAt n k r = winningAfter n k r - winningAfter n k (r-1)

x ./ y = fromRational (x % y)

expect_rounds n k = sum [ r * winningAt n k r | r <- [1..n] ] ./ sum [ winningAt n k r | r <- [1..n] ]

remaining card balls = length' . filter (not . (`elem` balls)) $ card

expect_rounds_card card balls = expect_rounds n' k' where
	n' = n - length' balls
	k' = remaining card balls

showColored card balls = "[" ++ concat ( intersperse "," (map (showOneColored balls) card ) ) ++ "]"

showOneColored balls card | card `elem` balls = green (show card)
			  | otherwise         = red (show card)

n = 90
k = 15

runCard card = runCard' card [] 

totalCards :: Integer -> Integer -> Integer -> Integer -> Rational
totalCards n k r g = ncr r g * ncr (n-r) (k-g) % ncr n k

length' = toInteger . length

showPct = (++"%") . show .  (/ 100) . fromIntegral . round . (*10000) . fromRational

readInt :: String -> IO Integer
readInt x = do
	putStr x
	inp <- getLine
	case (not.null) inp && all (`elem` "1234567890") inp of
		True -> return (read inp)
		False -> readInt x

dom :: Rational -> Double
dom p | p == 1 = 0
      | otherwise = fromRational (1 / (1 - p))

runCard' card balls = do
	replicateM_ 5 $ putStrLn ""
	when (lb == 0) . putStrLn . red $ "******** CARTON **********"
	putStrLn $ "Your card: " ++ showColored card balls
	putStrLn $ "Out balls: " ++ showColored [1..n] balls
	putStrLn $ "Good: " ++ show lg ++ "/" ++ show (lg + lb)
	putStrLn $ "Winning bracket: " ++ showPct win_min ++ " - " ++ showPct win_max
	putStrLn $ "Dominating: " ++ show (dom win_min) ++ " - " ++ show (dom win_max)
	putStrLn $ "Expected rounds to completion: " ++ show (expect_rounds_card card balls)
	sequence [ putStrLn $ "Odds to win after " ++ show k ++ " steps: " ++ showPct(odds_after k) |
		k <- [lb .. lb+3] ]
	sequence [ putStrLn $ "Estimated cards with " ++ show b ++ " holes: "
		++ showPct (totalCards n k r (k-b)) | b <- [1..5]]
	putStrLn ""

	nb <- readInt "Next ball:>"
	runCard' card (sort $ nb:balls) where
		(good, bad) = partition (`elem` balls) card
		r = length' balls
		lg = length' good
		lb = length' bad
		odds_after k = winningAfter (n - length' balls) lb k ./ ncr (n - length' balls) lb
		s_better_cards = sum [totalCards n k r g | g <- [(lg+1)..(k-1)]]
		better_cards = s_better_cards + totalCards n k r lg
		win_max = (1 - s_better_cards)
		win_min = (1 - better_cards)
		


mycards :: [[Integer]]
mycards = [
	[1,2,14,19,24,33,37,49,55,59,68,69,76,81,90],
	[7,8,18,25,29,30,39,40,57,59,65,72,78,86,90],
	[1,6,10,24,26,34,43,48,50,58,66,74,76,89,90],
	[4,6,15,24,25,32,37,42,56,59,62,64,79,88,90],
	[5,7,14,24,29,36,41,49,51,52,61,69,77,78,87],
	[8,9,14,19,20,34,38,46,47,53,55,61,74,81,87],
	[3,4,15,17,24,25,30,36,44,58,66,78,79,82,86],
	[6,7,11,12,24,35,37,44,58,59,60,66,76,89,90],
	[5,10,12,28,30,39,42,43,57,59,63,69,76,86,90],
	[8,9,18,19,24,31,36,45,49,55,66,67,75,77,89],
	[6,8,11,19,21,29,33,35,41,42,55,60,64,79,84],
	[3,5,14,17,21,24,39,46,49,54,66,73,76,84,88],
	[7,8,13,16,29,32,34,48,49,59,65,69,78,86,89],
	[5,9,15,19,24,28,35,40,42,52,67,69,79,88,90],
	[1,3,15,19,23,30,43,48,50,58,61,77,78,88,89],
	[2,3,14,22,29,38,39,49,58,59,67,71,78,82,88],
	[5,9,10,13,20,34,39,44,46,56,66,69,78,79,82],
	[8,14,17,25,29,37,39,45,48,55,68,69,78,79,87],
	[2,9,17,19,23,26,33,48,49,57,68,69,71,89,90],
	[5,6,12,21,28,37,45,46,57,58,62,72,75,89,90]]
