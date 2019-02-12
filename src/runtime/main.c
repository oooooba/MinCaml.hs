#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern intptr_t min_caml_start(void *sp, void *hp);
void *min_caml_hp;

intptr_t *min_caml_create_array(uintptr_t n, intptr_t x) {
    intptr_t *p = malloc(sizeof(intptr_t) * n);
    if (!p) {
        fprintf(stderr, "min_caml_create_array: allocation failed\n");
        abort();
    }
    for (uintptr_t i = 0; i < n; ++i) {
        p[i] = x;
    }
    return p;
}

int main(int argc, char *argv[]) {
    size_t len = 1024 * 1024;
    void *hp = malloc(len);
    void *sp = malloc(len);
    if (!hp || !sp) {
        return 1;
    }
    if (argc > 2 && strcmp(argv[1], "--test") == 0) {
        intptr_t expected = atoll(argv[2]);
        intptr_t actual = min_caml_start(sp, hp);
        return expected != actual;
    }
    min_caml_start(sp, hp);
    return 0;
}
