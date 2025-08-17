// Test for break/continue in deeply nested loops
// Tests break and continue behavior with 3+ levels of nesting
// Simplified to avoid overflow and infinite loop issues

int main(void) {
    int result = 0;
    
    // Test 1: Three levels with break statements
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            for (int k = 0; k < 3; k++) {
                result++;
                if (k == 1) {
                    break;  // Break innermost loop when k==1
                }
            }
            if (j == 1 && i == 0) {
                break;  // Break middle loop 
            }
        }
    }
    // Expected: result = 6
    
    // Test 2: Three levels with continue statements  
    int count = 0;
    for (int a = 0; a < 2; a++) {
        for (int b = 0; b < 3; b++) {
            if (b == 1) {
                continue;  // Skip b==1
            }
            for (int c = 0; c < 2; c++) {
                if (c == 0 && a == 1) {
                    continue;  // Skip c==0 when a==1
                }
                count++;
            }
        }
    }
    result = result + count;
    // Expected: count = 7, result = 13
    
    // Test 3: Mixed loop types with break/continue
    int sum = 0;
    int x = 0;
    while (x < 2) {
        for (int y = 0; y < 2; y++) {
            int z = 0;
            do {
                sum++;
                if (z == 0 && y == 1) {
                    z++;
                    continue;  // Skip rest of iteration
                }
                sum++;
                z++;
            } while (z < 2);
            if (y == 0 && x == 1) {
                break;  // Break for loop
            }
        }
        x++;
    }
    result = result + sum;
    // Expected: sum = 12, result = 25
    
    // Test 4: Five levels with early exit using goto
    int deep = 0;
    for (int l1 = 0; l1 < 2; l1++) {
        for (int l2 = 0; l2 < 2; l2++) {
            for (int l3 = 0; l3 < 2; l3++) {
                for (int l4 = 0; l4 < 2; l4++) {
                    for (int l5 = 0; l5 < 2; l5++) {
                        deep++;
                        if (deep == 10) {
                            goto exit_all;  // Use goto to exit all loops
                        }
                    }
                }
            }
        }
    }
    exit_all:
    result = result + deep;  // Add 10 to result
    // Expected: result = 25 + 10 = 35
    
    return result;
}