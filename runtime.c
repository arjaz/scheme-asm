#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#define fixnum_shift 2
#define fixnum_mask 0b11
#define fixnum_tag 0b00
#define char_shift 8
#define char_mask 0b11111111
#define char_tag 0b00001111
#define	bool_f 0b00101111
#define	bool_t 0b01101111
#define empty_list 0b00111111

typedef struct {
	void *eax;                  /* 0 scratch */
	void *ebx;                  /* 4 preserve */
	void *ecx;                  /* 8 scratch */
	void *edx;                  /* 12 scratch */
	void *esi;                  /* 16 preserve */
	void *edi;                  /* 20 preserve */
	void *ebp;                  /* 24 preserve */
	void *esp;                  /* 28 preserve */
} context_t;

typedef unsigned int scm_ptr;
scm_ptr scheme_entry(context_t*, char*, char*);

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

static char* allocate_protected_space(int size) {
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	/* Allocate requested size + 2 pages, allow reads and writes */
	char* p = mmap(0, aligned_size + 2 * page,
	               PROT_READ | PROT_WRITE,
	               MAP_ANONYMOUS | MAP_PRIVATE,
	               0, 0);
	if (p == MAP_FAILED) {
		printf("mmap failed\n");
		exit(-1);
	}
	/* protect the first page against reads and writes */
	status = mprotect(p, page, PROT_NONE);
	if (status != 0) {
		printf("mprotect of the first page failed\n");
		exit(-1);
	}
	/* protect the last page against reads and writes */
	status = mprotect(p + page + aligned_size, page, PROT_NONE);
	if (status != 0) {
		printf("mprotect of the last page failed\n");
		exit(-1);
	}
	return (p + page);
}

static void deallocate_proteced_space(char* p, int size) {
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	status = munmap(p - page, aligned_size + 2 * page);
	if (status != 0) {
		printf("munmap failed/n");
		exit(-1);
	}
}

int main(int argc, char** argv) {
	int stack_size = 16 * 4096; /* holds 16k cells */
	char* stack_top = allocate_protected_space(stack_size);
	char* stack_base = stack_top + stack_size;

	int heap_size = 16 * 4096;
	char* heap_base = allocate_protected_space(heap_size);

	context_t context;
	print_scm_ptr(scheme_entry(&context, stack_base, heap_base));

	deallocate_proteced_space(stack_top, stack_size);
	deallocate_proteced_space(heap_base, heap_size);

	return 0;
}
