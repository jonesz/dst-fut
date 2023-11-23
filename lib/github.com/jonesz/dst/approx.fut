import "bba"

module type approx = {
	type t [n]

	-- | Given focal elements that sum to less than < 1, normalize
	-- | them to 1.
	val normalize [n][w] : [w]t[n] -> [w]t[n]

	-- | Return the k largest BBAs within the focal element.
	val kx [n][w] : (k: i64) -> [w]t[n] -> [k]t[n]

	-- | Return the k largest BBAs within the focal element
	-- | with the rest union'd.
	val summarize [n][w] : (k: i64) -> [w]t[n] -> [k+1]t[n]
}

module mk_approx (B: bba): approx = {
	type t [n] = B.t[n]

	def normalize [w][n] (e: [w]t[n]): [w]t[n] =
		let sum = map (B.mass) e |> B.r.sum |> B.r.max (B.r.i64 1)
		in map (\i -> match i
				case #nil -> #nil
				case #elem (s, m) -> 
					let m_new = (B.r./) sum m
					in #elem (s, m_new)
				) e
	
	def kx [n][w] (k: i64) (e: [w]t[n]): [k]t[n] =
		let sorted = B.sort e
		-- if `k > w`, the arr is filled with `(k - w)` `#nil`s.
		in (if k > w
			then (replicate (k - w) B.nil) |> concat sorted :> [k]t[n]
			else take k (reverse sorted))
		|> normalize

	def summarize [n][w] (k: i64) (e: [w]t[n]): [k+1]t[n] =
		let f x =
			let r_sum = map 
				(\i -> match i 
					case #nil         -> B.r.i64 0
					case #elem (_, m) -> m) x
				|> reduce (B.r.+) (B.r.i64 0)

			-- TODO: There's issues with `bitset [n]` sizing.
			-- let r_set = map
			-- 	(\i -> match i
			-- 		case #nil         -> B.set.empty n
			-- 		case #elem (s, _) -> s) x
			-- 	|> reduce (B.set.union) (B.set.empty n)

			-- in #elem (r_set, r_sum)
			in #elem (???, r_sum)
	
		let sorted = B.sort e
		-- if `k > w` the arr is filled with `(k - w)` `#nil`s.
		in if k > w
			then (replicate ((k+1) - w) B.nil) |> concat sorted :> [k+1]t[n]
			else 
				take k sorted 
				|> concat [f (drop k sorted)] :> [k+1]t[n]
}
