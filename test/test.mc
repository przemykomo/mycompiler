int getnum();
void internal(int a, char b);
void exit (int __status);

public void _start() {

    int a = getnum();

    a++;

    char b = 'b';

    internal(a, b);

    int i = 0;
    exit(i);
}
