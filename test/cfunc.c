#include <stdio.h>

void internal(_Bool d) {
    printf("Internal function: %d\n", d);
}

long getnum() {
    return 10;
}
