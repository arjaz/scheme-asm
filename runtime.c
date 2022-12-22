#include <stdio.h>

#define fixnum_shift 2
#define fixnum_mask 0b11
#define fixnum_tag 0b00
#define char_shift 8
#define char_mask 0b11111111
#define char_tag 0b00001111
#define	bool_t 0b00101111
#define	bool_f 0b01101111
#define empty_list 0b00111111

typedef unsigned int scm_ptr;

static void print_scm_ptr(scm_ptr x) {
	if ((x & fixnum_mask) == fixnum_tag) {
		printf("%d", ((int) x) >> fixnum_shift);
	} else if ((x & char_mask) == char_tag) {
		printf("%c", ((int) x) >> char_shift);
	} else if (x == bool_f) {
		printf("#f");
	} else if (x == bool_t) {
		printf("#t");
	} else if (x == empty_list) {
		printf("()");
	} else {
		printf("#<unknown 0x%08x>", x);
	}
	printf("\n");
}

int main(int argc, char** argv) {
	print_scm_ptr(scheme_entry());
	return 0;
}
