#include <stdio.h>
#include <stdbool.h>

// unoptimized translation of input
unsigned int naiveCount() {
    unsigned short h = 0;
    for (long b = 108100; b < 125100; b += 17) {
        bool f = true;
        for (long d = 2; d < b; d++) {
            for (long e = 2; e < b; e++) {
                if (d * e == b) {
                    f = false;
                }
            }
        }
        h += !f;
    }
    return h;
}

unsigned int count() {
    unsigned short h = 0;
    for (long b = 108100; b <= 125100; b += 17) {
        bool f = true;
        for (unsigned short d = 2; f && d * d <= b; d++) {
            if (b % d == 0) {
                f = false;
            }
        }
        h += !f;
    }
    return h;
}

int main() {
    printf("%hu\n", count());
}