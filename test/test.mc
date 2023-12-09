extern internal;
extern exit;

public fn _start() {
    char a = 'a';
    char b = 'b';
    char c = 'c';

    if (a == 'a') {
        internal(a);
        char d = 'b';
        if (b == d) {
            internal(d);
        }
    }

    if (c > 'd') {
        internal(c);
    }

    if (c < 'a') {
        internal(a);
    }

    int i = 0;
    exit(i);
}
