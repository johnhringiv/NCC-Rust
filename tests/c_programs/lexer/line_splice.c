// Backslash-newline joins two lines before tokenizing (C translation phase 2). The splice
// can fall between tokens, inside a token, or at the end of a // comment or # line, which then
// continues onto the next line. Returns 21.
#define IGNORED this directive \
   continues here
int main(void) {
    int x = 1; // this comment ends in a splice, so the next line is comment too \
    x = 100;
    int y = 2\
0;
    re\
turn x \
+ y;
}
