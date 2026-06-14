// Regression: LONG_MIN written with an explicit `L` suffix must parse.
//
// parse_factor folds a leading unary `-` into the literal string only for
// `ConstantInt` tokens. The unsuffixed form `-9223372036854775808` is rescued
// by the int->long overflow-promotion path, but the `L`-suffixed form produced
// a `ConstantLong` token that skipped the fold, so parse_constant tried
// `parse::<i64>("9223372036854775808")` (2^63, > i64::MAX) and errored with
// "does not fit in 64-bit int". Both spellings must yield LONG_MIN.
int main(void) {
    long suffixed = -9223372036854775808L;
    long unsuffixed = -9223372036854775808;     // works via int->long promotion
    long computed = -9223372036854775807L - 1;  // (LONG_MIN + 1) - 1 = LONG_MIN
    return (suffixed == unsuffixed) + (suffixed == computed);  // 2 when correct
}
