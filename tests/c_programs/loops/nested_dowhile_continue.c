// Test for continue in nested do-while loops
// Tests the behavior of continue statements in various nested do-while configurations

int main(void) {
    int count = 0;
    
    // Test 1: Continue in inner do-while
    int i = 0;
    do {
        int j = 0;
        do {
            j++;
            if (j == 2) {
                continue;  // Skip when j == 2
            }
            count++;
        } while (j < 4);
        i++;
    } while (i < 2);
    // Inner loop: j=1(count++), j=2(continue), j=3(count++), j=4(count++)
    // Runs twice (i=0,1), so count = 3 * 2 = 6
    
    // Test 2: Continue in outer do-while
    int k = 0;
    do {
        k++;
        if (k == 2) {
            continue;  // Skip rest when k == 2
        }
        int m = 0;
        do {
            count++;
            m++;
        } while (m < 2);
    } while (k < 3);
    // k=1: inner runs twice (count += 2)
    // k=2: continue (inner doesn't run)
    // k=3: inner runs twice (count += 2)
    // count = 6 + 2 + 2 = 10
    
    // Test 3: Multiple continues at different levels
    int n = 0;
    do {
        n++;
        int p = 0;
        do {
            p++;
            if (p == 1 && n == 2) {
                continue;  // Continue inner when p==1 and n==2
            }
            if (p == 3) {
                count++;
                continue;  // Always continue when p==3
            }
            count += 2;
        } while (p < 3);
        
    } while (n < 2);
    // n=1: p=1(count+=2), p=2(count+=2), p=3(count++)
    // n=2: p=1(continue), p=2(count+=2), p=3(count++)
    // count = 10 + 5 + 3 = 18
    
    // Test 4: Triple nested do-while with continue
    int x = 0;
    do {
        x++;
        int y = 0;
        do {
            y++;
            int z = 0;
            do {
                z++;
                if (z == 2) {
                    continue;  // Skip in innermost loop
                }
                count++;
            } while (z < 3);
        } while (y < 1);
    } while (x < 2);
    // Innermost: z=1(count++), z=2(continue), z=3(count++)
    // Middle runs once (y<1), outer runs twice (x<2)
    // count = 18 + (2 * 1 * 2) = 22
    
    return count;
}