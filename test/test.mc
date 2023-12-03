extern internal;
extern exit;

public fn _start() {
    let test = 5;
    internal(test);
    test++;
    exit(test);
}
