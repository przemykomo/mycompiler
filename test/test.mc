void other(int a);

struct potega {
    int a,
    int b
}

public int main() {
    potega test;
    test.a = 1;
    test.b = 20;
    test.a = test.b - 4;
    int i = test.a + 3 - test.b / 4;
    other(i);

    return 0;
}
