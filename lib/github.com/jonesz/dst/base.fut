module type base = {
	-- | Type to represent an element within the universe.
	type t

	-- | Whether the element is the empty set.
	val is_empty     : t -> bool
	-- | Whether two sets are equal.
	val is_eq        : t -> t -> bool
	-- | Whether the first is a subset of the second.
	val is_subset    : t -> t -> bool
	-- | Compute the intersection of two sets.
	val intersection : t -> t -> t

	-- | Compute the inverse of the set..
	val not : t -> t

	-- The following `num_bit` / `get_bit` are required for radix sorting.

	-- | Number of bits within the type; corresponds to the cardinality of the
	-- | singleton set.
	val num_bits : i32
	-- | Whether the i'th element of the singleton set is a member of the passed set.
	val get_bit  : i32 -> t -> i32
}

module mk_base_integral(X: integral): base with t = X.t = {
	type t = X.t

	def ne = X.i64 0i64
	def not = X.not

	def is_empty a = X.i64 0 |> (X.==) a
	def is_eq a b = (X.^) a b |> is_empty 
	def intersection a b = (X.&) a b
	def is_subset a b = (a X.& b) |> (X.== a)

	def num_bits = X.num_bits
	def get_bit = X.get_bit
}
