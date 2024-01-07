void other(int a);
int another(int a, int b) {
    return a + b;
}

struct potega {
    int a,
    int b
}

public int main() {

    int a = 5;
    int b = 10;
    a = another(a, b);

    other(a);

    return 0;
}
