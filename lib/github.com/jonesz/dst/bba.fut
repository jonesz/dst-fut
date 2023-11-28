-- | Basic Belief Assumptions.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "../../diku-dk/containers/bitset"
import "../../diku-dk/sorts/merge_sort"

module type bba = {
	-- | Set representation in a BBA.
	type u [n]
	module u_set: bitset

	-- | Mass representation in a BBA.
	type m
	module m_real   : {
		include real with t = m
	}

	-- | Tuple of (u, m).
	-- Left abstract because there's a chance we'd like to pad
	-- this with nonsense.
	type t [n]

	-- | Make a `t` value from `u[n]` and `m`.
	val mk    [n] : u[n] -> m -> t[n]
	-- | Return the `nil` element.
	val nil       : (n: i64) -> t[(n - 1) / u_set.nbs + 1]

	-- | Convert a list of indices and a mass into a type `t`.
	val i64_m   [z] : (n: i64) -> [z]i64 -> m -> t[(n - 1) / u_set.nbs + 1]

	-- | Return the set representation of a focal element `t[n]`.
	val set  [n] : t[n] -> u[n]
	-- | Return the mass of a focal element `t`.
	val mass [n] : t[n] -> m

	-- | Sort each focal element from least mass to most mass.
	val sort [n][x] : [x]t[n] -> [x]t[n]
}

-- | A BBA under a CWA hypothesis; that is the empty set contains
-- | mass zero.
module mk_bba_cwa (U: bitset) (M: real): bba with u[n] = U.bitset[n] with m = M.t with t[n] = (U.bitset[n], M.t) = {
	type u[n] = U.bitset[n]
	module u_set = U

	type m = M.t
	module m_real = M

	type t[n] = (U.bitset[n], m)

	def mk a b = (a, b)
	def nil n  = 
		((U.empty n), M.i64 0)

	def i64_m n z m =
		mk (U.from_array n z) m

	def set  [n] (e: t[n]) = e.0
	def mass [n] (e: t[n]) = e.1

	def sort e = 
		merge_sort_by_key (mass) (M.<=) e
}
