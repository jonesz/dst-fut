import "dst"
import "set"
import "comb"

module d = mk_dst(mk_set_integral(i32))
module c = mk_comb(mk_set_integral(i32))

-- The movie example from "Example producing correct results in case of high conflict"
-- https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

-- ==
-- entry: comb_dempster_i32
-- input  { [ 4, 2, 1, 2 ] [ 0.99f64, 0.01, 0.99, 0.01] 2 }
-- output { 1.0f64 }
entry comb_dempster_i32 s m q = 
  	let tmp = zip s m
  	let (e1, e2) = (tmp[0:2], tmp[2:4])
  	in d.bel (c.comb_dempster e1 e2) q
