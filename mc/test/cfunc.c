#include <stdint.h>
#include <stdio.h>

struct MyStruct {
    int32_t a;
    int16_t c;
    float d;
    float e;
    int64_t l;
};

struct Small {
    int32_t a;
    float b;
    float c;
};

struct MyStruct abi_second();

void cfun() {
    struct MyStruct var = abi_second();
    printf("a: %i, c: %i, d: %f, e: %f, l: %li", var.a, var.c, var.d, var.e,
           var.l);
}

struct Small arg_cfun(struct MyStruct var, int32_t b, float c, struct Small s) {
    printf("a: %i, c: %i, d: %f, e: %f, l: %li\n", var.a, var.c, var.d, var.e,
           var.l);
    printf("b: %i, c: %f\n", b, c);
    printf("a: %i, b: %f, c: %f\n", s.a, s.b, s.c);
    return s;
}
