extern internal;
extern exit;

public fn _start() {
    char a = 'a';
    char b = 'b';
    char c = 'c';

    if (a == 'c') {
        internal(a);
    } else {
        internal(c);
    }

    int i = 0;
    exit(i);
}
