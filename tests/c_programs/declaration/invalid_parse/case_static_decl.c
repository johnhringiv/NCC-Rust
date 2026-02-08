// Case label followed by static declaration - should fail to parse
int main(void) {
    switch (1) {
    case 1:
        static int x = 5;
        return x;
    }
    return 0;
}