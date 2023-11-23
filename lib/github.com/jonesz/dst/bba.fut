import "../../diku-dk/sorts/merge_sort"
import "../../diku-dk/containers/bitset"

module type bba = {
	module set : bitset

	type u [n] = set.bitset [n]
	type m

	module r   : {
		include real with t = m
	}

	-- | A BBA is a tagged (set, mass) tuple pair.
	type t [n] = #nil | #elem (u[n], m)

	-- | Return the nil representation.
	val nil [n] : t[n]

	-- | Return the mass within a value `t[n]`.
	val mass [n] : t[n] -> m

	-- | Sort each bitset from least to most.
	val sort [n][x] : [x]t[n] -> [x]t[n]
	-- | Return all values that are `#elem`.
	val elem [n][x] : [x]t[n] -> []t[n]
}

module mk_bba (U: bitset) (M: real): bba with m = M.t = {
	module set = U
	module r   = M

	type u [n] = set.bitset [n]
	type m = M.t
	type t [n] = #nil | #elem (u[n], m)

	def nil [n]: t[n] = #nil

	def mass [n] (e: t[n]) =
		match e
			case #nil         -> M.i64 0
			case #elem (_, m) -> m

	def sort [n][x] (e: [x]t[n]) = merge_sort_by_key 
		(\x -> match x
			case #nil          -> M.i64 0
			case #elem (_s, m) -> m) 
		(M.<=) e

	def elem [n][x] (e: [x]t[n]) =
		filter 
			(\i -> match i
				case #nil    -> false
				case #elem _ -> true) 
			e
}
