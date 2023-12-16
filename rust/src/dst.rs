pub trait DST {
    /// The basic belief assignment.
    type B;
    /// The query.
    type Q;

    fn bel(assignment: &Self::B, q: &Self::Q) -> f64;
    fn pl(assignment: &Self::B, q: &Self::Q) -> f64;
}
