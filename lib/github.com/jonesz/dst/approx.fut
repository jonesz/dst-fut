import "bba"
import "../../diku-dk/containers/bitset"

module type approx = {
	module u_set : bitset
	type t [n] 

	-- | Given focal elements that sum to less than < 1, normalize
	-- | them to 1.
	val normalize [n][w] : [w]t[(n - 1) / u_set.nbs + 1] -> [w]t[(n - 1) / u_set.nbs + 1]

	-- | Return the k largest BBAs within the focal element.
	val kx [n][w] : (k: i64) -> [w]t[(n - 1) / u_set.nbs + 1] -> [k]t[(n - 1) / u_set.nbs + 1]

	-- | Return the k largest BBAs within the focal element
	-- | with the rest union'd.
	val summarize [n][w] : (k: i64) -> [w]t[(n - 1) / u_set.nbs + 1] -> [k+1]t[(n - 1) / u_set.nbs + 1]
}

module mk_approx (B: bba): approx with t[n] = B.t[n] = {
	module u_set = B.u_set
	type t [n] = B.t[n]

	def normalize [w][n] (e: [w]t[(n - 1) / B.u_set.nbs + 1]): [w]t[(n - 1) / B.u_set.nbs + 1] =
		let total = map (B.mass) e |> reduce (B.m_real.+) (B.m_real.i64 0)
		in map (\i -> B.mk (B.set i) ((B.m_real./) (B.mass i) total)) e
	
	def kx [n][w] (k: i64) (e: [w]t[(n - 1) / u_set.nbs + 1]): [k]t[(n - 1) / u_set.nbs + 1] =
		let sorted = B.sort e |> reverse
		in (if k > w
			then (++) sorted (replicate (k - w) B.nil) :> [k]t[(n - 1) / u_set.nbs + 1]
			else take k sorted
		) |> normalize

	def summarize [n][w] (k: i64) (e: [w]t[(n - 1) / B.u_set.nbs + 1]): [k+1]t[(n - 1) / B.u_set.nbs + 1] =
		let f (a: []t[(n - 1) / B.u_set.nbs + 1]): t[(n - 1) / B.u_set.nbs + 1] = 
			let b = map (B.set)  a |> reduce (B.u_set.union) (B.set B.nil)
			let c = map (B.mass) a |> reduce (B.m_real.+) (B.m_real.i64 0)
			in B.mk b c

		let sorted = B.sort e |> reverse
		-- if k is larger than w, we need to pad with nil.
		in if k > w
			then (++) sorted (replicate ((k + 1) - w) B.nil) :> [k+1]t[(n - 1) / B.u_set.nbs + 1]
			else (++) (take k sorted) [(f (drop k sorted))]
}
