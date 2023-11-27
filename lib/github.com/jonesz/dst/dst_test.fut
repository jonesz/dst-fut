import "dst"
import "bba"
import "comb"
import "../../diku-dk/containers/bitset"

-- The Schroedinger's cat example from
-- https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

module bi32 = mk_bba_cwa (mk_bitset i32) f64
module di32 = mk_dst (mk_bba_cwa (mk_bitset i32) f64)

-- ==
-- entry: bel_i32
-- input  { [ 2i64, 1, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.2f64, 0.5, 1.0 ] }
entry bel_i32 [n] s m =
	let e = map2 (bi32.i64_m n) s m
	in map (di32.bel e) (map (bi32.mass) e)

-- ==
-- entry: pl_i32
-- input  { [ 2, 1, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.5, 0.8, 1.0 ] }
entry pl_i32 s m =
	let e = zip s m
	in map (di32.pl e) s

-- The movie example from "Example producing correct results in case of high conflict"
-- https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

-- ==
-- entry: comb_dempster_i32
-- input  { [ 4, 2, 1, 2 ] [ 0.99f64, 0.01, 0.99, 0.01] 2 }
-- output { 1.0f64 }
entry comb_dempster_i32 s m q = 
  	let tmp = zip s m
  	let (e1, e2) = (tmp[0:2], tmp[2:4])
  	in di32.bel (di32.comb_dempster e1 e2) q
