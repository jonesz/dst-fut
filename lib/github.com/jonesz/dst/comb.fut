import "base"
import "../../diku-dk/sorts/radix_sort"

module type comb = {
	-- | Type to represent a set within the universe.
	type t
	-- | Scalar type to represent mass.
	type m

	val comb_dempster [b][c] : [b](t, m) -> [c](t, m) -> [](t, m)
}

module mk_comb_integral(X: integral) (M: real): comb with t = X.t with m = M.t = {
	type t = X.t
	type m = M.t

	module B = mk_base_integral X

	def comb_dempster b c =
		let K =
	 		map (\(b_set, b_mass) ->
	 			map (\(c_set, c_mass) ->
	 				let cond = B.intersection b_set c_set |> B.is_empty
	 				in if cond
	 					then (M.*) b_mass c_mass
	 					else (M.i32 0)
	 				) c
	 			) b |> flatten |> reduce (M.+) (M.i32 0)

	 	let conflict = (M.-) (M.i32 1) K |> (M./) (M.i32 1)

	 	-- We compute the intersections of B/C resulting in [A]. [A] must
	 	-- be reduced to a set; remove the duplicate sets in [A] to form A.
	 	-- Then for each value in A, sum the corresponding values in [A].

	 	-- Compute [A], sorted.
	 	let A_dup =
	 		map (\(b_set, b_mass) ->
	 			map (\(c_set, c_mass) ->
	 				let a_set = B.intersection b_set c_set
	 				let cond  = B.is_empty a_set |> not
	 				in if cond
	 					then (a_set, (M.*) b_mass c_mass)
	 					else (a_set, (M.i32 0))
	 			) c
	 		) b |> flatten

	 	-- Compute the keys of A.
	 	let A_dedup_key =
	 		let keys = map (.0) A_dup
	 		in zip3 (indices keys) keys (rotate (-1) keys)
	 		|> filter (\(i,x,y) -> i == 0 || (B.is_eq x y |> not))
	 		|> map (.1)
	 		|> radix_sort B.num_bits B.get_bit -- TODO: Bug here.


	 	let A = map (\k ->
	 		map (\(l_set, l_mass) -> 
	 			if B.is_eq k l_set
	 				then l_mass
	 				else (M.i32 0)
	 			) A_dup |> reduce (M.+) (M.i32 0)
	 		) A_dedup_key |> map (M.* conflict) |> zip A_dedup_key

	 	in filter (\(_, m) -> ((M.>) m (M.i32 0))) A
}
