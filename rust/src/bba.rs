mod bitset {
    trait SetOperations {
        type S;
        /// Compute the union between two sets.
        fn union(a: &Self::S, b: &Self::S) -> Self::S;
        /// Compute the intersection between two sets.
        fn intersection(a: &Self::S, b: &Self::S) -> Self::S;
        /// Compute whether 'a' is a subset of 'b'.
        fn is_subset(a: &Self::S, b: &Self::S) -> bool;
    }
}
