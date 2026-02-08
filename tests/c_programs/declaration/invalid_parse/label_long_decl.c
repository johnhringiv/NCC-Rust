// Label followed by long declaration - should fail to parse
int main(void) {
label:
    long x = 5;
    return 0;
}