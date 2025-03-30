struct mystruct {
    int a,
    int b
}

void other(mystruct* buf);

public int main() {
    mystruct a = mystruct {
        a: 5,
        b: 3
    };

    other(&a);

    return 0;
}

int myfunc() {
    return 1;
}
