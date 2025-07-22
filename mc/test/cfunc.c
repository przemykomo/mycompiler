#include <stdint.h>
#include <stdio.h>
struct mystruct {
    int64_t a;
    int64_t b;
};

void other(struct mystruct* arg) {
    printf("a = %ld, b = %ld\n", arg->a, arg->b);

    // struct mystruct a = {.a = 5, .b = 3};
}

void mytest() {
    printf("hello\n");
}

void print_num(long long a) {
    printf("num: %lli \n", a);
}
