int getnum();
void internal(int a, int b, char c);

int mynum() {
    return 55;
}

public int main() {
    int a = getnum();
    int b = mynum();

    a++;
    b--;

    char c = 'b';

    internal(a, b, c);

    return 25;
}
