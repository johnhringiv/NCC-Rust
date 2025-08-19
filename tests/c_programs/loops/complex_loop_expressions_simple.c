// Simplified test for complex expressions in loop headers
// Uses only features supported by the compiler grammar

int main(void) {
    int total = 0;
    int x = 10;
    int y = 5;
    int z = 8;
    
    // Test 1: Complex condition with ternary operator
    for (int j = 0; j < (x > y ? z : x); j++) {
        total++;
    }
    // x > y is true, so loop runs while j < z (8 times)
    // total = 8
    
    // Test 2: Assignment in initialization
    int a = 0;
    for (int i = (a = 3); i < 6; i++) {
        total = total + i;
    }
    // i: 3, 4, 5
    // total = 8 + 3 + 4 + 5 = 20
    
    // Test 3: Compound assignment in post-expression
    int k;
    for (k = 0; k < 9; k += 3) {
        total = total + k;
    }
    // k: 0, 3, 6
    // total = 20 + 0 + 3 + 6 = 29
    
    // Test 4: Postfix increment in condition
    int count = 0;
    int limit = 3;
    while (count++ < limit) {
        total++;
    }
    // count: 0->1 (total++), 1->2 (total++), 2->3 (total++), 3->4 (exit)
    // total = 29 + 3 = 32
    
    // Test 5: Prefix decrement in condition
    int down = 3;
    while (--down) {
        total = total + down;
    }
    // down: 3->2 (total+=2), 2->1 (total+=1), 1->0 (exit)
    // total = 32 + 2 + 1 = 35
    
    return total;
}