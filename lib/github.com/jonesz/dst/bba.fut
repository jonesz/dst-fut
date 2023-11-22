import "../../diku-dk/sorts/radix_sort"

module type set = {
	type t

	val empty : t
}

module type bba = {
	type u
	type m

	type t = #id | #elem (u, m)

	val universe : t -> u
	val mass : t -> m

	val neutral_element : t
	val sort [x] : [x]t -> [x]t
}

module mk_bba (U: bitset) (M: real): bba with u = U.t with m = M.t = {
	type u = U.t
	type m = M.t

	type t = #id | #elem (u, m)

	def universe (z: t): u = 
		match z
			case #elem (u, _) -> u
			case #id -> U.empty

	def mass (z: t): m = 
		match z
			case #id -> M.i64 0
			case #elem (_, m) -> m

	def neutral_element: t = #id
	def sort z = ???
}
