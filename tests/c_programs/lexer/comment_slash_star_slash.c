// The sequence slash-star-slash opens a block comment but does not close it: the star belongs
// to the opener. The comment below runs until the second slash-star-slash, hiding `+ 1`, so
// this returns 1. (The header uses line comments since the sequence would end a block comment.)
int main(void) {
    return 1 /*/ + 1 /*/ ;
}
