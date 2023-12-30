import "dst"
import "set"

-- The Schroedinger's cat example from
-- https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

module d = mk_dst(mk_set_integral(i32))

-- ==
-- entry: bel_i32
-- input  { [ 1, 2, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.2f64, 0.5, 1.0 ] }
entry bel_i32 s m =
	map (d.bel (zip s m)) s

-- ==
-- entry: pl_i32
-- input  { [ 1, 2, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.5f64, 0.8, 1.0 ] }
entry pl_i32 s m =
	map (d.pl (zip s m)) s
