import "approx"
import "bba"
import "../../diku-dk/containers/bitset"

module approx_i32 = mk_approx (mk_bba (mk_bitset i32) f64)
