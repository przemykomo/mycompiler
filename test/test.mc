extern internal;
extern exit;

public fn _start() {
    char c = 'd';
    int addr = &c;
    char d = *addr;
    internal(c, addr, d);
    int i = 0;
    exit(i);
}
