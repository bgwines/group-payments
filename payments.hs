
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O

length'' :: [a] -> Double
length'' = fromIntegral . length

from_just :: Maybe a -> a
from_just (Just x) = x

-- (item name, buyer name, recipients, cost)
src_data :: [(String, String, [String], Double)]
src_data = [
	("TV",                "Tom",   ["Tom", "Lucas", "Brett"], 1000),
	("Rug",               "Brett", ["Tom", "Lucas", "Brett"], 300),
	("Swedish Fish",      "Lucas", ["Brett"],                 3),
	("Laundry Detergent", "Lucas", ["Lucas", "Tom"],          10)]

src_data' :: [(String, [String], Double)]
src_data' = map (\(_, b, c, d) -> (b, c, d)) src_data

who_owes_whom_how_much :: [(String, String, Double)]
who_owes_whom_how_much =
	filter h . map g . L.group . L.sort . concat . map f $ src_data'
	where
		f (buyer, owers, amount) =
			[(ower, buyer, amount / (length'' owers)) | ower <- owers]
		g = foldl1 (\(a, b, r) (a', b', r') -> (a, b, r + r'))
		h (a, b, _) = a /= b

who_owes_how_much :: [(String, Double)]
who_owes_how_much =
	reverse
		. L.sortBy (O.comparing snd)
			. M.toList
				. foldl update_record M.empty
					$ who_owes_whom_how_much
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
