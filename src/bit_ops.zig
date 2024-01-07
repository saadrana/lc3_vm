pub fn sign_extend(val: u16, bit_count: u4) u16 {
    // check if sign bit is 1
    // if true, populate extra bits with one instead of zero
    // else the value is already good to go

    var temp_val = val;
    if ((temp_val >> (bit_count - 1) & 0x1 == 1)) {
        const all_ones: u16 = 0xFFFF;
        temp_val |= (all_ones << bit_count);
    }
    return temp_val;
}
