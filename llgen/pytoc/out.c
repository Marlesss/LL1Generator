#include <stdio.h>
#include <stdbool.h>

int main() {
    bool b;
    b = true;
    int a;
    a = 12;
    for (int i = 1; (i <= 3); ++i) {
        if (b) {
            printf("%s", "abab\n");
        } else {
            printf("%s", "abacaba\n");

    }
    if ((1 < 4)) {
        if ((((4 != 6)) && ((5 == ((1 + 4)))))) {
            printf("%s", "ABC\n");
        }
    }
    int x;
    scanf("%d", &x);
    printf("%d", ((x + (5 * 2)) - 4));
    while ((((x > 0)) && (((4 + 1) < 10)))) {
        printf("%d", x);
        x = (x - 1);
    }
}
