#include <stdio.h>
#include <stdlib.h>

extern void min_caml_start(void *sp, void *hp);
void *min_caml_hp;

int main(int argc, char *argv[]) {
    size_t len = 1024 * 1024;
    void *hp = malloc(len);
    void *sp = malloc(len);
    if (!hp || !sp) {
        return 1;
    }
    printf("hp=%p, sp=%p\n", hp, sp);
    min_caml_start(sp, hp);
    return 0;
}
