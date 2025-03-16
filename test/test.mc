void other(int a);

struct mystruct {
    int a,
    int b
}

public int main() {
    int a = 5;
    int b = 5;
    ++b;

    other(a);
    other(b);

    return 0;
}
