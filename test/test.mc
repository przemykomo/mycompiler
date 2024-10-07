void other(int a);

public int main() {
    int a = 0;
    while (a < 5) {
        ++a;
        other(a);
    }

    return 0;
}
