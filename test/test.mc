int getnum();
void internal(bool d);

int mynum() {
    return 55;
}

public int main() {
    int a = getnum();
    int b = mynum();

    a++;
    b--;

    bool test = a == b;
    bool test2 = a < b;
    bool test3 = true;
    bool test4 = false;

    char c = 'b';

    internal(test);
    internal(test2);
    internal(test3);
    internal(test4);

    return 25;
}
