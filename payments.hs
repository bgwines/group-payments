
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord

import Data.Maybe

-- (item name, buyer name, recipients, cost)
src_data :: String
src_data =
	"TV                Tom   [Tom,Lucas,Brett] 1000\n" ++
	"Rug               Brett [Tom,Lucas,Brett] 300\n" ++
	"SwedishFish       Lucas [Brett]           3\n" ++
	"LaundryDetergent  Lucas [Lucas,Tom]       10"

src2 :: String
src2 =
	"_ F [E] 10\n" ++
	"_ E [D] 9\n" ++
	"_ D [C] 8\n" ++ 
	"_ C [B] 8\n" ++ 
	"_ B [A] 13\n"

calc_owings_to_people :: [(String, [String], Double)] -> [(String, String, Double)]
calc_owings_to_people
	= filter (\(a, b, _) -> a /= b)
	. map (foldl1 g)
	. List.group
	. List.sort
	. concatMap f
	where
		f :: (String, [String], Double) -> [(String, String, Double)]
		f (buyer, owers, amount)
			= [(ower, buyer, amount') | ower <- owers]
			where
				amount' :: Double
				amount' = amount / (length' owers)

		g :: (String, String, Double) -> (String, String, Double) -> (String, String, Double)
		g (a, b, r) (_, _, r') = (a, b, r + r')

		length' :: [a] -> Double
		length' = fromIntegral . length


calc_individual_owings :: [(String, String, Double)] -> [(String, Double)]
calc_individual_owings
	= reverse
	. List.sortBy (Ord.comparing snd)
	. filter doesnt_owe_zero
	. Map.toList
	. foldl update_record Map.empty
	where
		doesnt_owe_zero :: (String, Double) -> Bool
		doesnt_owe_zero (_, n) = (n /= 0)

		update_record :: Map.Map String Double -> (String, String, Double) -> Map.Map String Double
		update_record m record@(a, b, r) = m''
			where
				s = Map.lookup a m
				t = Map.lookup b m

				s' = r
				t' = r * (-1)

				-- TODO: this, but more monadically somehow?
				s'' = if isNothing s then s' else (fromJust s) + s'
				t'' = if isNothing t then t' else (fromJust t) + t'

				m'  = Map.insert a s'' m
				m'' = Map.insert b t'' m'

calc_optimal_payment_strategy :: [(String, [String], Double)] -> Map.Map String (String, Double)
calc_optimal_payment_strategy formatted_input
	= Map.fromList as_list
	where
		individual_owings :: [(String, Double)]
		individual_owings
			= calc_individual_owings
			. calc_owings_to_people
			$ formatted_input

		as_list :: [(String, (String, Double))]
		as_list = zipWith f individual_owings (tail individual_owings)

		f :: (String, Double) -> (String, Double) -> (String, (String, Double))
		f (a, e) (b, _) = (a, (b, e))

format :: String -> [(String, [String], Double)]
format = map format_line . lines
	where
		format_line :: String -> (String, [String], Double)
		format_line
			= (\(a, b, c, d) -> (b, g c, read' d))
			. to_tuple4
			. words

		to_tuple4 :: [String] -> (String, String, String, String)
		to_tuple4 l = (l !! 0, l !! 1, l !! 2, l !! 3)
		
		g :: String -> [String]
		g = Split.splitOn "," . tail . init

		read' :: String -> Double
		read' d = read d :: Double

main :: IO ()
main = do
	contents <- getContents
	putStrLn . show $ calc_optimal_payment_strategy . format $ contents
