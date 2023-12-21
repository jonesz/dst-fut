pub trait DST {
    /// The basic belief assignment.
    type B;
    /// The query.
    type Q;

    fn bel(m: &Self::B, q: &Self::Q) -> f64;
    fn pl(m: &Self::B, q: &Self::Q) -> f64;
}
