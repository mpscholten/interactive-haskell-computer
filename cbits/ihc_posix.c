#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/stat.h>

int ihc_o_rdonly(void) { return O_RDONLY; }
int ihc_o_wronly(void) { return O_WRONLY; }
int ihc_o_rdwr(void) { return O_RDWR; }
int ihc_o_append(void) { return O_APPEND; }
int ihc_o_creat(void) { return O_CREAT; }
int ihc_o_excl(void) { return O_EXCL; }
int ihc_o_trunc(void) { return O_TRUNC; }
#ifdef O_NOCTTY
int ihc_o_noctty(void) { return O_NOCTTY; }
#else
int ihc_o_noctty(void) { return 0; }
#endif
#ifdef O_NONBLOCK
int ihc_o_nonblock(void) { return O_NONBLOCK; }
#else
int ihc_o_nonblock(void) { return 0; }
#endif
#ifdef O_BINARY
int ihc_o_binary(void) { return O_BINARY; }
#else
int ihc_o_binary(void) { return 0; }
#endif

int ihc_s_isreg(unsigned int m) { return S_ISREG(m); }
int ihc_s_ischr(unsigned int m) { return S_ISCHR(m); }
int ihc_s_isblk(unsigned int m) { return S_ISBLK(m); }
int ihc_s_isdir(unsigned int m) { return S_ISDIR(m); }
int ihc_s_isfifo(unsigned int m) { return S_ISFIFO(m); }

size_t ihc_sizeof_stat(void) { return sizeof(struct stat); }
unsigned int ihc_st_mode(const struct stat *s) { return s->st_mode; }
uint64_t ihc_st_dev(const struct stat *s) { return (uint64_t)s->st_dev; }
uint64_t ihc_st_ino(const struct stat *s) { return (uint64_t)s->st_ino; }
