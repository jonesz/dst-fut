-- | Basic Belief Assumptions.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "../../diku-dk/containers/bitset"
import "../../diku-dk/sorts/merge_sort"

module type bba = {
	module u_set : bitset

	-- | Mass representation of a BBA.
	type m
	module m_real   : {
		include real with t = m
	}

	type t [n]

	-- | Make a `t` value from (bitset[n], m).
	val mk   [n] : u_set.bitset[(n - 1) / u_set.nbs + 1] -> m -> t[(n - 1) / u_set.nbs + 1]
	-- | Return the `nil` element.
	val nil  [n] : t[(n - 1) / u_set.nbs + 1]

	-- | Return the set representation of a focal element `t[n]`.
	val set  [n] : t[(n - 1) / u_set.nbs + 1] -> u_set.bitset[(n - 1) / u_set.nbs + 1]
	-- | Return the mass of a focal element `t[n]`.
	val mass [n] : t[(n - 1) / u_set.nbs + 1] -> m

	-- | Sort each focal element from least mass to most mass.
	val sort [n][x] : [x]t[(n - 1) / u_set.nbs + 1] -> [x]t[(n - 1) / u_set.nbs + 1]
}

-- | A BBA under a CWA hypothesis; that is the empty set contains
-- | mass zero.
module mk_bba_cwa (U: bitset) (M: real): bba with m = M.t with t[n] = (U.bitset[n], M.t) = {
	module u_set = U

	type m = M.t
	module m_real = M

	type t [n] = (U.bitset[n], m)

	def mk   a b = (a, b)
	def nil  [n] = ((U.empty n), M.i64 0)

	def set  [n] (e: t[n]): U.bitset[n] = e.0
	def mass [n] (e: t[n]): m    = e.1

	def sort e = 
		merge_sort_by_key (mass) (M.<=) e
}
