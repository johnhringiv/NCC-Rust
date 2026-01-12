// Collatz Conjecture Explorer
//
// For any positive integer n:
//   - If n is even: n = n / 2
//   - If n is odd:  n = 3n + 1
// The conjecture states this always reaches 1.
//
// Which number under 100 takes the most steps?

static int total_steps = 0;

int collatz(int n) {
    int steps = 0;
    while (n != 1) {
        if (n & 1) {
            n = 3 * n + 1;    // odd
        } else {
            n = n >> 1;       // even: divide by 2
        }
        steps++;
    }
    total_steps += steps;
    return steps;
}

int main(void) {
    int champion = 1;
    int max_steps = 0;

    for (int i = 1; i < 100; i++) {
        int steps = collatz(i);
        if (steps > max_steps) {
            max_steps = steps;
            champion = i;
        }
    }

    // Returns 97: takes 118 steps to reach 1!
    // 97 -> 292 -> 146 -> 73 -> 220 -> 110 -> 55 -> ... -> 1
    return champion;
}
