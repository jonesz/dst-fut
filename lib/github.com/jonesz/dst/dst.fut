--| Reasoning with uncertainty via Dempster-Shafer Theory.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "base"
import "comb"
import "../../diku-dk/sorts/radix_sort"

module type dst = {
	-- | Type to represent a set within the universe.
	type t
	-- | Scalar type to represent mass.
	type m

 	-- | Given a subset of the frame of discernment, compute the the belief.
 	val bel [f] : [f](t, m) -> t -> m
 	-- | Given a subset of the frame of discernment, compute the the plausability.
 	val pl  [f] : [f](t, m) -> t -> m
 
 	-- We avoid irregular arrays here by requiring the frame of discernment to be of a certain length.
 	-- rule, neutral_element, then the sets of FoD. TODO: Is the actual "neutral element" a tagged type.
    -- val comb_reduce [f][z] : ([f](t, m) -> [f](t, m) -> [f](t, m)) -> [f](t, m) -> [z][f](t, m) -> [f](t, m)

	include comb with t = t with m = m
}

module mk_dst_integral(X: integral) (M: real): dst with t = X.t with m = M.t = {
	type t = X.t
	type m = M.t

	module B = mk_base_integral X
	module C = mk_comb_integral X M

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

	open C
}
