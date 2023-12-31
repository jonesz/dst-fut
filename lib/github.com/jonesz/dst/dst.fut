import "set"

module type dst = {
	type s
	type t = (s, f64)

 	-- | Given a subset of the frame of discernment, compute the the belief.
 	val bel [f] : [f]t -> s -> f64
 	-- | Given a subset of the frame of discernment, compute the the plausability.
 	val pl  [f] : [f]t -> s -> f64

	-- | Combine a set of masses together with a combination rule and an approximation scheme.
	val comb [f][a][b] : [f][a]t -> ([a]t -> [a]t -> [b]t) -> ([b]t -> [a]t) -> [a]t
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

	-- https://futhark-lang.org/examples/no-neutral-element.html
	type with_neutral [a] 't = #neutral | #val [a]t

	def comb [f][a] e rl apx =
		let e_tagged: [f]with_neutral [a]t = map (\z -> #val z) e
		let m3 = reduce (\m1 m2 ->
		  		match (m1, m2)
		  			case (#val b, #val c) -> #val (rl b c |> apx)
		  			case (#neutral, _) -> m2
		  			case (_, #neutral) -> m1
			) #neutral e_tagged
		in match m3 
		case #neutral -> replicate a (X.nil, 0.0f64)
		case #val x   -> x
}
