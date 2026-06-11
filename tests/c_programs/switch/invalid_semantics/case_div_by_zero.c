/* A division-by-zero in a case label is a constant-expression error (exit 30),
   reported specifically as "division by zero in constant expression" rather than the
   generic "not an integer constant expression". */
int main(void) {
    switch (1) {
        case 5 / 0:
            return 1;
    }
    return 0;
}
