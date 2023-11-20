--| Reasoning with uncertainty via Dempster-Shafer Theory.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "../../diku-dk/sorts/radix_sort"

module type DST = {
 	-- | Type to represent an set within the universe.
 	type t
	-- | Scalar type to represent mass.
 	type m

	-- | Type to represent a basic belief assignment.
	type s
 
 	-- | Given a subset of the frame of discernment, compute the the belief.
 	val bel [f] : [f](t, m) -> t -> m
 	-- | Given a subset of the frame of discernment, compute the the plausability.
 	val pl  [f] : [f](t, m) -> t -> m
 
 	-- Combine two frames of discernment with a rule.
 	-- val comb [b][c][a] : ([b](t, m) -> [c](t, m) -> [a](t, m)) -> [b](t, m) -> [c](t, m) -> [a](t, m)
 
 	-- We avoid irregular arrays here by requiring the frame of discernment to be of a certain length.
 	-- rule, neutral_element, then the sets of FoD. TODO: Is the actual "neutral element" a tagged type.
    -- val comb_reduce [f][z] : ([f](t, m) -> [f](t, m) -> [f](t, m)) -> [f](t, m) -> [z][f](t, m) -> [f](t, m)

	val comb_dempster [b][c] : [b](t, m) -> [c](t, m) -> [](t, m)
}

module type base = {
	-- | Type to represent an element within the universe.
	type t

	-- | Whether the element is the empty set.
	val is_empty     : t -> bool
	-- | Whether two sets are equal.
	val is_eq        : t -> t -> bool
	-- | Whether the first is a subset of the second.
	val is_subset    : t -> t -> bool
	-- | Compute the intersection of two sets.
	val intersection : t -> t -> t

	-- | Compute the inverse of the set..
	val not : t -> t

	-- The following `num_bit` / `get_bit` are required for radix sorting.

	-- | Number of bits within the type; corresponds to the cardinality of the
	-- | singleton set.
	val num_bits : i32
	-- | Whether the i'th element of the singleton set is a member of the passed set.
	val get_bit  : i32 -> t -> i32
}

module mk_DST (B: base) (M: real): DST with s = (B.t, M.t) = {
	type t = B.t
	type m = M.t
	type s = (t, m)

	def bel e q =
		map (\(b_set, b_mass) -> 
			let cond = B.is_subset b_set q
			in if cond
				then b_mass
				else (M.i32 0)
		) e |> reduce (M.+) (M.i32 0)

	def pl e q =
		-- pl(Q) = 1 - bl(not Q)
		B.not q |> bel e |> (M.-) (M.i32 1)

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

module mk_base_bool(X: { val x: i64 }): base = {
	type t = [X.x]bool

	-- Note: we need to define `is_empty` here because we need to define
	-- `not` later.
	def is_empty a = or a |> not
	def is_eq a b = map2 (\y z -> (==) (i32.bool y) (i32.bool z)) a b |> and
	def intersection a b = map2 (&&) a b 
	def is_subset a b = map2 (\ai bi -> ai && bi |> (==) ai) a b |> and

	def ne = replicate X.x false
	def not a = map not a

	def num_bits = i32.i64 X.x
	def get_bit i t = i32.bool t[i]
}

module mk_base_integral(X: integral): base = {
	type t = X.t

	def ne = X.i64 0i64
	def not = X.not

	def is_empty a = X.i64 0 |> (X.==) a
	def is_eq a b = (X.^) a b |> is_empty 
	def intersection a b = (X.&) a b
	def is_subset a b = (a X.& b) |> (X.== a)

	def num_bits = X.num_bits
	def get_bit = X.get_bit
}
