extern internal;
extern exit;

public fn _start() {
    int[5] a;
    a[0] = 0;
    a[1] = 1;
    a[2] = 24;
    a[3] = 55;
    a[4] = 120;
    internal(a);
    int i = 0;
    exit(i);
}
