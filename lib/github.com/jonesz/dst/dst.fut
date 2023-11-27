--| Reasoning with uncertainty via Dempster-Shafer Theory.
-- Ethan Jones <etn.jones@gmail.com>, 2023.

import "../../diku-dk/containers/bitset"
import "bba"

module type dst = {
	module u_set : bitset

	type m
	type t [n]

 	-- | Given a subset of the frame of discernment, compute the the belief.
 	val bel [n][f] : [f]t[(n - 1) / u_set.nbs + 1] -> u_set.bitset[(n - 1) / u_set.nbs + 1] -> m
 	-- | Given a subset of the frame of discernment, compute the the plausability.
 	val pl  [n][f] : [f]t[(n - 1) / u_set.nbs + 1] -> u_set.bitset[(n - 1) / u_set.nbs + 1] -> m
}

module mk_dst(B: bba): dst with m = B.m with t[n] = B.t[n] = {
	module u_set = B.u_set

	type m = B.m
	type t [n] = B.t [n]

	def bel e q =
		map (B.set) e
		|> map (\p 
			-> if (u_set.is_subset p q)
				then B.m_real.i64 1
				else B.m_real.i64 0
		) |> map2 (B.m_real.*) (map (B.mass) e)
		|> B.m_real.sum

	def pl e q =
		-- pl(Q) = 1 - bl(not Q)
		u_set.complement q |> bel e |> (B.m_real.-) (B.m_real.i64 1)
}
