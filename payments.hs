
import qualified Data.List.Split as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O

length'' :: [a] -> Double
length'' = fromIntegral . length

from_just :: Maybe a -> a
from_just (Just x) = x

-- (item name, buyer name, recipients, cost)
src_data :: String
src_data =
	"TV                Tom   [Tom,Lucas,Brett] 1000\n" ++
	"Rug               Brett [Tom,Lucas,Brett] 300\n" ++
	"SwedishFish       Lucas [Brett]           3\n" ++
	"LaundryDetergent  Lucas [Lucas,Tom]       10"

who_owes_whom_how_much :: [(String, [String], Double)] -> [(String, String, Double)]
who_owes_whom_how_much =
	filter h . map g . L.group . L.sort . concat . map f
	where
		f (buyer, owers, amount) =
			[(ower, buyer, amount / (length'' owers)) | ower <- owers]
		g = foldl1 (\(a, b, r) (a', b', r') -> (a, b, r + r'))
		h (a, b, _) = a /= b

who_owes_how_much :: [(String, String, Double)] -> [(String, Double)]
who_owes_how_much =
	reverse
		. L.sortBy (O.comparing snd)
			. M.toList
				. foldl update_record M.empty
	where update_record m record@(a, b, r) = m''
		where
			s = M.lookup a m
			t = M.lookup b m

			s' = r
			t' = r * (-1)

			-- TODO: this, but more monadically somehow?
			s'' = if s == Nothing then s' else (from_just s) + s'
			t'' = if t == Nothing then t' else (from_just t) + t'

			m'  = M.insert a s'' m
			m'' = M.insert b t'' m'

calc_optimal_payment_strategy :: [(String, [String], Double)] -> M.Map String (String, Double)
calc_optimal_payment_strategy formatted_input = M.fromList as_list
	where
		l = who_owes_how_much $ who_owes_whom_how_much $ formatted_input
		as_list = zipWith (\(a, e) (b, _) -> (a, (b, e))) l (tail l)

read' :: String -> Double
read' d = read d :: Double

format :: String -> [(String, [String], Double)]
format = map format_line . lines
	where
		format_line = f . to_tuple4 . words
		to_tuple4 l = (l !! 0, l !! 1, l !! 2, l !! 3)
		f (a, b, c, d) = (b, g c, read' d)
		g = S.splitOn "," . tail . init

main = do
	contents <- getContents
	putStrLn . show $ calc_optimal_payment_strategy (format contents)
