#include <stdint.h>
#include <stdio.h>

struct MyStruct {
    int32_t a;
    int16_t c;
    float d;
    float e;
    int64_t l;
};

struct MyStruct abi_second();

void cfun() {
    struct MyStruct var = abi_second();
    printf("a: %i, c: %i, d: %f, e: %f, l: %li", var.a, var.c, var.d, var.e,
           var.l);
}
