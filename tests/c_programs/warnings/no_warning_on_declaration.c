/* Function declarations should not trigger unused parameter warnings */
int putchar(int c);

int main(void) {
    putchar(65);
    return 0;
}
