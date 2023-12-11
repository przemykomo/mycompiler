#include <stdio.h>

void internal(long a, long b, char c) {
    printf("Internal function: %ld %ld %c hello\n", a, b, c);
}

long getnum() {
    return 123;
}
