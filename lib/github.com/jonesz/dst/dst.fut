import "set"

module type dst = {
	type s
	type t = (s, f64)

 	-- | Given a subset of the frame of discernment, compute the the belief.
 	val bel [f] : [f]t -> s -> f64
 	-- | Given a subset of the frame of discernment, compute the the plausability.
 	val pl  [f] : [f]t -> s -> f64
}

module mk_dst(X: set): dst with s = X.t = {
	type s = X.t
	type t = (s, f64)

	def bel (e: []t) q =
		map (.0) e
		|> map (\z 
			-> if (X.is_subset z q)
				then 1.0f64
				else 0.0f64
		) |> map2 (f64.*) (map (.1) e)
		|> f64.sum

	def pl [f] (e: [f]t) (q: s): f64 =
		-- pl(Q) = 1 - bl(not Q)
		X.not q |> bel e |> (f64.-) 1.0f64
}
