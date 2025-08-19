// Test break at middle level of three-level loop

int main(void) {
    int result = 0;
    
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            for (int k = 0; k < 2; k++) {
                result++;
            }
            if (j == 1 && i == 0) {
                break;  // Break middle loop when i==0, j==1
            }
        }
    }
    // i=0: j=0(k=0,1), j=1(k=0,1,break) -> 4 iterations
    // i=1: j=0(k=0,1), j=1(k=0,1), j=2(k=0,1) -> 6 iterations
    // Total: 10
    
    return result;
}