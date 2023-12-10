#include <stdio.h>

void internal(long a, char b) {
    printf("Internal function: %ld %c hello\n", a, b);
}

long getnum() {
    return 123;
}
