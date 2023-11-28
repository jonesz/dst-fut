import "bba"
import "../../diku-dk/containers/bitset"

module bba_i32 = mk_bba_cwa (mk_bitset i32) f64

-- ==
-- entry: bba_sort
-- input  { [[0i64], [1i64], [2i64]] [0.2f64, 0.5, 0.3] }
-- output { [0.5f64, 0.3f64, 0.2f64] }
entry bba_sort [x] (u: [x][]i64) (m: [x]f64) =
	map2 (bba_i32.i64_m 32) u m
	|> bba_i32.sort
	|> map (bba_i32.mass) 
	|> reverse

-- ==
-- entry: bba_nil
-- input  { }
-- output { 0.0f64 }
entry bba_nil =
	bba_i32.nil 32 |> bba_i32.mass
