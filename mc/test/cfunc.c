#include <stdint.h>
#include <stdio.h>
struct mystruct {
    int64_t a;
    int64_t b;
};

void other(struct mystruct arg) {
    printf("a = %ld, b = %ld\n", arg.a, arg.b);

    // struct mystruct a = {.a = 5, .b = 3};
}

void mytest() { printf("hello\n"); }

int64_t add(int64_t a, int64_t b) { return a + b; }

void print_num(int64_t a, int64_t b) { printf("a: %li, b: %li", a, b); }

void yes() { printf("Yes\n"); }

void no() { printf("No\n"); }

struct grzybex {
    int64_t a;
    int32_t b;
    float f;
    double g;
};

struct grzybex abi_test(int32_t a, int8_t b, int64_t c, float d, double e) {
    return (struct grzybex){.a = c, .b = a, .f = d, .g = e};
}
