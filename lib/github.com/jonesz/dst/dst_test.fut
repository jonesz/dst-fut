import "dst"

module bool_2 = mk_base_bool{ def x = 2i64 }
module dbool_2 = mk_DST bool_2 f64
module bool_3 = mk_base_bool{ def x = 3i64 }
module dbool_3 = mk_DST bool_3 f64

-- The Schroedinger's cat example from
-- https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

-- ==
-- entry: bel_bool
-- input  { [ [true, false], [false, true], [true, true] ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.2, 0.5, 1.0 ] }
entry bel_bool s m =
	let e = zip s m
 	in map (dbool_2.bel e) s

-- ==
-- entry: pl_bool
-- input  { [ [true, false], [false, true], [true, true] ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.5, 0.8, 1.0 ] }
entry pl_bool s m =
	let e = zip s m
	in map (dbool_2.pl e) s

-- ==
-- entry: comb_dempster_bool
-- input  { [ [true, false, false], [false, true, false], [false, false, true], [false, true, false] ] [ 0.99f64, 0.01, 0.99, 0.01] }
-- output { [ 1.0f64 ] }
entry comb_dempster_bool s m = 
  	let tmp = zip s m
  	let (e1, e2) = (tmp[0:2], tmp[2:4])
  
  	let e = dbool_3.comb_dempster e1 e2
  	in dbool_3.bel e [false, true, false]
 
module i32_set = mk_base_integral(i32)
module di32 = mk_DST i32_set f64

-- ==
-- entry: bel_i32
-- input  { [ 2, 1, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.2, 0.5, 1.0 ] }
entry bel_i32 s m =
	let e = zip s m
	in map (di32.bel e) s

-- ==
-- entry: pl_i32
-- input  { [ 2, 1, 3 ] [0.2f64, 0.5, 0.3] }
-- output { [ 0.5, 0.8, 1.0 ] }
entry pl_i32 s m =
	let e = zip s m
	in map (di32.pl e) s
