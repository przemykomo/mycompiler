void other(int a);

struct mystruct {
    int a,
    int b
}

public int main() {
    mystruct a = mystruct {
        a: 5,
        b: 3
    };

    other(a.a);
    other(a.b);

    return 0;
}
