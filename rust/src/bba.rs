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

    pub(super) struct BitSet<const N: usize>
    where
        [(); (N / std::mem::size_of::<u8>()) + 1]:,
    {
        buf: [u8; (N / std::mem::size_of::<u8>()) + 1],
    }

    impl<const N: usize> BitSet<N>
    where
        [(); (N / std::mem::size_of::<u8>()) + 1]:,
    {
        /// Given a u8 buf, return the wrapped BitSet struct.
        pub fn from_buf_u8(buf: [u8; (N / std::mem::size_of::<u8>()) + 1]) -> Self {
            BitSet { buf }
        }
    }

    impl<const N: usize> SetOperations for BitSet<N>
    where
        [(); (N / std::mem::size_of::<u8>()) + 1]:,
    {
        type S = Self;

        /// Compute the union between two sets.
        fn union(a: &Self::S, b: &Self::S) -> Self::S {
            todo!();
        }

        /// Compute the intersection between two sets.
        fn intersection(a: &Self::S, b: &Self::S) -> Self::S {
            todo!();
        }

        /// Compute whether 'a' is a subset of 'b'.
        fn is_subset(a: &Self::S, b: &Self::S) -> bool {
            todo!();
        }
    }
}

pub struct BBA<const N: usize>
where
    [(); (N / std::mem::size_of::<u8>()) + 1]:,
{
    bba: Vec<(bitset::BitSet<N>, f64)>,
}
