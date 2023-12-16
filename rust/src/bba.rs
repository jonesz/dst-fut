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
        /// Wrap a BitSet struct around something that can be turned into the apppropiate
        /// sized buf.
        pub fn from_buf(buf: impl Into<[u8; (N / std::mem::size_of::<u8>()) + 1]>) -> Self {
            BitSet { buf: buf.into() }
        }

        fn buf(&self) -> &[u8; (N / std::mem::size_of::<u8>()) + 1] {
            &self.buf
        }
    }

    impl<const N: usize> SetOperations for BitSet<N>
    where
        [(); (N / std::mem::size_of::<u8>()) + 1]:,
    {
        type S = Self;

        /// Compute the union between two sets.
        fn union(a: &Self::S, b: &Self::S) -> Self::S {
            let mut z = [0u8; (N / std::mem::size_of::<u8>()) + 1];
            for (idx, mem) in z.iter_mut().enumerate() {
                *mem = a.buf().get(idx).unwrap() | b.buf().get(idx).unwrap();
            }
            BitSet::<N>::from_buf(z)
        }

        /// Compute the intersection between two sets.
        fn intersection(a: &Self::S, b: &Self::S) -> Self::S {
            let mut z = [0u8; (N / std::mem::size_of::<u8>()) + 1];
            for (idx, mem) in z.iter_mut().enumerate() {
                *mem = a.buf().get(idx).unwrap() & b.buf().get(idx).unwrap();
            }
            BitSet::<N>::from_buf(z)
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
