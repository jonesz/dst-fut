use super::dst::DST;

pub mod bitset {
    pub trait SetOperations {
        type S;

        /// Compute the union between two sets.
        fn union(a: &Self::S, b: &Self::S) -> Self::S;
        /// Compute the intersection between two sets.
        fn intersection(a: &Self::S, b: &Self::S) -> Self::S;
        /// Compute whether 'a' is a subset of 'b'.
        fn is_subset(a: &Self::S, b: &Self::S) -> bool;
        /// Return the bitwise not.
        fn not(a: &Self::S) -> Self::S;
        /// Return whether two sets are equal.
        fn eq(a: &Self::S, b: &Self::S) -> bool;
    }

    pub struct BitSet<const N: usize>
    where
        [(); (N / std::mem::size_of::<usize>()) + 1]:,
    {
        buf: [usize; (N / std::mem::size_of::<usize>()) + 1],
    }

    impl<const N: usize> BitSet<N>
    where
        [(); (N / std::mem::size_of::<usize>()) + 1]:,
    {
        /// Wrap a BitSet struct around something that can be turned into the apppropiate
        /// sized buf.
        pub fn from_buf(buf: impl Into<[usize; (N / std::mem::size_of::<usize>()) + 1]>) -> Self {
            BitSet { buf: buf.into() }
        }

        fn buf(&self) -> &[usize; (N / std::mem::size_of::<usize>()) + 1] {
            &self.buf
        }
    }

    impl<const N: usize> SetOperations for BitSet<N>
    where
        [(); (N / std::mem::size_of::<usize>()) + 1]:,
    {
        type S = Self;

        /// Compute the union between two sets.
        fn union(a: &Self::S, b: &Self::S) -> Self::S {
            let mut z = [0usize; (N / std::mem::size_of::<usize>()) + 1];
            for (idx, mem) in z.iter_mut().enumerate() {
                *mem = a.buf().get(idx).unwrap() | b.buf().get(idx).unwrap();
            }
            BitSet::<N>::from_buf(z)
        }

        /// Compute the intersection between two sets.
        fn intersection(a: &Self::S, b: &Self::S) -> Self::S {
            let mut z = [0usize; (N / std::mem::size_of::<usize>()) + 1];
            for (idx, mem) in z.iter_mut().enumerate() {
                *mem = a.buf().get(idx).unwrap() & b.buf().get(idx).unwrap();
            }
            BitSet::<N>::from_buf(z)
        }

        /// Compute whether 'a' is a subset of 'b'.
        fn is_subset(a: &Self::S, b: &Self::S) -> bool {
            a.buf()
                .iter()
                .zip(b.buf())
                .map(|(a, b)| a & b == *a)
                .all(|x| x)
        }

        /// Return the bitwise not.
        fn not(a: &Self::S) -> Self::S {
            let mut z = [0usize; (N / std::mem::size_of::<usize>()) + 1];
            for (idx, mem) in z.iter_mut().enumerate() {
                *mem = !(a.buf().get(idx).unwrap())
            }

            // Only indices that are <= cardinality of the set should be
            // flipped; mask the others off.
            let mut mask = 0usize;
            for i in 0..(N % std::mem::size_of::<usize>()) {
                mask |= 1 << i
            }

            *z.last_mut().unwrap() &= mask;
            BitSet::<N>::from_buf(z)
        }

        /// Return whether two sets are equal.
        fn eq(a: &Self::S, b: &Self::S) -> bool {
            a.buf().iter().zip(b.buf()).map(|(a, b)| a == b).all(|x| x)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_union() {
            let a = BitSet::<5>::from_buf([0b01110]);
            let b = BitSet::<5>::from_buf([0b10001]);
            let a_union_b = BitSet::<5>::union(&a, &b);

            assert!(BitSet::<5>::eq(
                &a_union_b,
                &BitSet::<5>::from_buf([0b11111])
            ));

            assert!(!BitSet::<5>::eq(&a_union_b, &a))
        }

        #[test]
        fn test_intersection() {
            let a = BitSet::<5>::from_buf([0b01110]);
            let b = BitSet::<5>::from_buf([0b10001]);
            let a_intersection_b = BitSet::<5>::intersection(&a, &b);

            assert!(BitSet::<5>::eq(
                &a_intersection_b,
                &BitSet::<5>::from_buf([0b00000])
            ));

            let c = BitSet::<5>::from_buf([0b01010]);
            let a_intersection_c = BitSet::<5>::intersection(&a, &c);
            assert!(BitSet::<5>::eq(&a_intersection_c, &c))
        }

        #[test]
        fn test_is_subset() {
            let a = BitSet::<5>::from_buf([0b01110]);
            let b = BitSet::<5>::from_buf([0b10001]);
            assert!(!BitSet::<5>::is_subset(&a, &b));

            let c = BitSet::<5>::from_buf([0b10000]);
            assert!(BitSet::<5>::is_subset(&c, &b));
        }

        #[test]
        fn test_not() {
            let a = BitSet::<5>::from_buf([0b01110]);
            let a_not = BitSet::<5>::not(&a);
            assert!(BitSet::<5>::eq(&a_not, &BitSet::<5>::from_buf([0b10001])));
        }
    }
}

pub struct BBA<const N: usize>
where
    [(); (N / std::mem::size_of::<usize>()) + 1]:,
{
    bba: Vec<(bitset::BitSet<N>, f64)>,
}

impl<const N: usize> DST for BBA<N>
where
    [(); (N / std::mem::size_of::<usize>()) + 1]:,
{
    type Q = bitset::BitSet<N>;
    type B = BBA<N>;

    fn bel(m: &Self::B, q: &Self::Q) -> f64 {
        use bitset::SetOperations;
        m.bba
            .iter()
            .map(|(m_s, m_m)| {
                if bitset::BitSet::<N>::is_subset(m_s, q) {
                    *m_m
                } else {
                    0.0f64
                }
            })
            .sum()
    }

    fn pl(m: &Self::B, q: &Self::Q) -> f64 {
        use bitset::SetOperations;
        1.0f64 - Self::bel(m, &Self::Q::not(q))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bel() {
        let MASS: BBA<3> = BBA {
            bba: vec![
                (bitset::BitSet::<3>::from_buf([0b100]), 0.35f64),
                (bitset::BitSet::<3>::from_buf([0b010]), 0.25f64),
                (bitset::BitSet::<3>::from_buf([0b001]), 0.15f64),
                (bitset::BitSet::<3>::from_buf([0b110]), 0.06f64),
                (bitset::BitSet::<3>::from_buf([0b101]), 0.05f64),
                (bitset::BitSet::<3>::from_buf([0b011]), 0.04f64),
                (bitset::BitSet::<3>::from_buf([0b111]), 00.1f64),
            ],
        };

        assert_eq!(
            0.35,
            BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b100]))
        );

        assert_eq!(
            0.25,
            BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b010]))
        );

        assert_eq!(
            0.15,
            BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b001]))
        );

        assert!(
            (0.66 - BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b110]))).abs() < 0.01
        );

        assert!(
            (0.55 - BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b101]))).abs() < 0.01
        );

        assert!(
            (0.44 - BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b011]))).abs() < 0.01
        );

        assert!((1.0 - BBA::<3>::bel(&MASS, &bitset::BitSet::<3>::from_buf([0b111]))).abs() < 0.01);
    }
}
