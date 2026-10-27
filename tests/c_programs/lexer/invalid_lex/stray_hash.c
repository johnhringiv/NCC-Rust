/* `#` only starts a (skipped) directive as the first character on a line. Anywhere else it is a
   stray character, not a comment that swallows the rest of the line. */
int main(void) {
    return 2 # junk
    ;
}
