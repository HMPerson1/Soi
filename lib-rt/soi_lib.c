#define _POSIX_C_SOURCE 200809L

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>

void printStr(const char *str) {
  fputs(str, stdout);
}

void printInt(int64_t i) {
  printf("%" PRId64, i);
}

static char *buf = NULL;
static size_t bufSize = 0;
int64_t readInt() {
  ssize_t i = getline(&buf, &bufSize, stdin);
  if (i < 0) exit(EXIT_FAILURE);

  char *end;
  errno = 0;
  intmax_t val = strtoimax(buf, &end, 10);
  if ((errno == ERANGE && (val == INTMAX_MAX || val == INTMAX_MIN))
      || (errno != 0 && val == 0)
      || (end == buf)
      || (val > INT64_MAX || val < INT64_MIN)
      ) {
    exit(EXIT_FAILURE);
  } else {
    return val;
  }
}
