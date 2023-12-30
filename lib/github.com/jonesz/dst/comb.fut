import "../../diku-dk/sorts/radix_sort"
import "set"

module type comb = {
	type s
	type t = (s, f64)

	val comb_dempster [b][c] : [b]t -> [c]t -> []t
}

module mk_comb(X: set): comb with s = X.t = {
	type s = X.t
	type t = (s, f64)

	def comb_dempster b c =
		let K =
	 		map (\(b_set, b_mass) ->
	 			map (\(c_set, c_mass) ->
	 				let cond = X.intersection b_set c_set |> X.is_empty
	 				in if cond
	 					then (f64.*) b_mass c_mass
	 					else (0.0f64)
	 				) c
	 			) b |> flatten |> reduce (f64.+) 0.0f64

	 	let conflict = (f64.-) 1.0f64 K |> (f64./) 1.0f64

		-- Compute all the intersections of B/C forming A, where A is an arr
		-- potentially containing duplicates. Sort A, then remove the duplicates, to
		-- form { A }. Then for each value within { A }, sum the corresponding values
		-- in A.

	 	let A =
	 		map (\(b_set, b_mass) ->
	 			map (\(c_set, c_mass) ->
	 				let a_set = X.intersection b_set c_set
	 				let cond  = X.is_empty a_set |> not
	 				in if cond
	 					then (a_set, (f64.*) b_mass c_mass)
	 					else (a_set, 0.0f64)
	 			) c
	 		) b |> flatten

	 	-- Compute the keys of A, removing duplicates.
	 	let A_set_keys =
	 		let sorted_keys = map (.0) A |> radix_sort X.num_bits X.get_bit
	 		in zip3 (indices sorted_keys) sorted_keys (rotate (-1) sorted_keys)
	 		|> filter (\(i,x,y) -> i == 0 || (X.is_eq x y |> not))
	 		|> map (.1)

		-- For each value in A_set_keys, sum the equivalent keys in A.
	 	let A_set = map (\a_k ->
	 		map (\(l_k, l_m) -> 
				if X.is_eq a_k l_k
	 				then l_m
	 				else 0.0f64
	 			) A 
				|> reduce (f64.+) 0.0f64
	 		) A_set_keys 
		|> map (f64.* conflict) |> zip A_set_keys

		-- Remove zero masses.
		-- TODO: I don't think this is necessarily needed; especially if we do some
		-- approximation/padding scheme to force a known-sized arr.
	 	in filter (\(_, m) -> (f64.>) m 0.0f64) A_set
}
