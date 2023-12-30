import "set"
import "../../diku-dk/sorts/merge_sort"

module type approx = {
	type s
	type t = (s, f64)

	-- | Return the k largest BBAs within the focal element.
	val kx [w] : (k: i64) -> [w]t -> [k]t

	-- | Return the k largest BBAs within the focal element with the rest union'd.
	val summarize [w] : (k: i64) -> [w]t -> [k+1]t
}

module mk_approx(X: set): approx with s = X.t = {
	type s = X.t
	type t = (s, f64)

	def normalize (e: []t): []t =
		let total = map (.1) e |> f64.sum
		in map (\z -> (f64./) z total) (map (.1) e) |> zip (map (.0) e)

	def kx [f] (k: i64) (e: [f]t): [k]t =
		let sorted = merge_sort_by_key (.1) (f64.<=) e |> reverse
		-- if k is larger than w, we need to pad with `nil`.
		-- TODO: We're working under CWA; does `nil` work in OWA?
		in (if k > f
			then (++) sorted (replicate (k - f) (X.nil, 0.0f64)) :> [k]t
			else take k sorted
		) |> normalize

	def summarize [f] (k: i64) (e: [f]t): [k+1]t =
		-- Given a set of focal elements, compute the union/sum of their sets/masses.
		let sm (a: []t): t = 
			let a_union = map (.0) a |> reduce (X.union) (X.nil)
			let a_sum   = map (.1) a |> f64.sum
			in (a_union, a_sum)

		let sorted = merge_sort_by_key (.1) (f64.<=) e |> reverse
		-- if k is larger than w, we need to pad with nil.
		-- TODO: We're working under CWA; does `nil` work in OWA?
		in if k > f
			then (++) sorted (replicate ((k + 1) - f) (X.nil, 0.0f64)) :> [k+1]t
			else (++) (take k sorted) [(sm (drop k sorted))]
}
