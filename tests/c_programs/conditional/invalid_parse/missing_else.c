int main(void) {
    // Missing else expression - should fail
    return 1 ? 42 : ;
}