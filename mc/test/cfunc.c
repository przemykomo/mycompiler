#include <stdint.h>
#include <stdio.h>
struct mystruct {
    int64_t a;
    int64_t b;
};

void other(struct mystruct *arg) {
    printf("a = %ld, b = %ld\n", arg->a, arg->b);

    // struct mystruct a = {.a = 5, .b = 3};
}

void mytest() { printf("hello\n"); }

int64_t add(int64_t a, int64_t b) { return a + b; }

void print_num(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
               int64_t f) {
    printf("a: %li, b: %li, c: %li, d: %li, e: %li, f: %li \n", a, b, c, d, e,
           f);
}

void yes() {
    printf("Yes\n");
}

void no() {
    printf("No\n");
}
