-- | Basic Belief Assumptions.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "../../diku-dk/containers/bitset"
import "../../diku-dk/sorts/merge_sort"

module type bba = {
	module u_set : bitset
	-- | Set representation of a BBA.
	type u [n] = u_set.bitset [n]

	-- | Mass representation of a BBA.
	type m
	module m_real   : {
		include real with t = m
	}

	type t [n]

	-- | Return the `nil` element.
	val nil  [n] : t[(n - 1) / u_set.nbs + 1]

	-- | Return the set representation of a focal element `t[n]`.
	val set  [n] : t[(n - 1) / u_set.nbs + 1] -> u[(n - 1) / u_set.nbs + 1]
	-- | Return the mass of a focal element `t[n]`.
	val mass [n] : t[(n - 1) / u_set.nbs + 1] -> m

	-- | Sort each focal element from least mass to most mass.
	val sort [n][x] : [x]t[n] -> [x]t[n]
}

-- | A BBA under a CWA hypothesis; that is the empty set contains
-- | mass zero.
module mk_bba_cwa (U: bitset) (M: real): bba with m = M.t with u[n] = U.bitset[n] with t[n] = (U.bitset[n], M.t) = {
	type u [n] = U.bitset [n]
	module u_set = U

	type m = M.t
	module m_real = M

	type t [n] = (u[n], m)

	def nil  [n] = ((U.empty n), M.i64 0)

	def set  [n] (e: t[n]): u[n] = e.0
	def mass [n] (e: t[n]): m    = e.1

	def sort e = 
		merge_sort_by_key (mass) (M.<=) e
}
