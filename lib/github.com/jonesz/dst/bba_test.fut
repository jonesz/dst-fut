import "bba"
import "../../diku-dk/containers/bitset"

module bba_i32 = mk_bba (mk_bitset i32) f64

-- ==
-- entry: bba_sort
-- input  { [[0i64], [1i64], [2i64]] [0.2f64, 0.5, 0.3] }
-- output { [0.5f64, 0.3f64, 0.2f64] }
entry bba_sort [x] (u: [x][]i64) (ma: [x]f64) =
	map2 (\m s -> #elem (s, m))
		ma (map (bba_i32.set.from_array 3) u)
	|> bba_i32.sort
	|> bba_i32.elem
	|> map (bba_i32.mass) 
	|> reverse
