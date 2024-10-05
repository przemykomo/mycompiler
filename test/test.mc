void other(int a);

public int main() {
    int a = 0;
    while (a < 5) {
        a = a + 1;
        other(a);
    }

    return 0;
}
