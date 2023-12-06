#include <stdio.h>
void internal(char a, char* b, char c) {
    printf("Internal function: %c %c %c hello\n", a, *b, c);
}
