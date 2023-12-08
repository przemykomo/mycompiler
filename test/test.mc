extern internal;
extern exit;

public fn _start() {
    char* a = "testowy string lets go";
    char b = *a;
    int c = 2 + 4;
    int d = c + 8;
    int g = d / c - 3;
    internal(a);
    int i = 0;
    exit(i);
}
