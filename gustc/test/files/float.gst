void main() {
    float x = 0;
    float y = 0.0;
    assert(x == y);

    x = 2.0;
    y = 1;
    assert(y / x != 3.14);
}
