import "../../diku-dk/sorts/merge_sort"
import "../../diku-dk/containers/bitset"

module type bba = {
	module set : bitset

	type u [n] = #id | #elem (set.bitset [n])
	type m

	val sort [n][x] : [x](u[n], m) -> [x](u[n], m)
}

module mk_bba (U: bitset) (M: real): bba with m = M.t = {
	module set = U

	type u [n] = #id | #elem (set.bitset [n])
	type m = M.t

	def sort = merge_sort_by_key (\(_, m) -> m) (M.<=)
}
