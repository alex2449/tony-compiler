#include <inttypes.h>
#include <stdio.h>

// output functions

void tony_puti(int32_t i) {
  printf("%d", i);
}

void tony_putb(int8_t b) {
  printf("%s", b ? "true" : "false");
}

void tony_putc(int8_t c) {
  printf("%c", c);
}

void tony_puts(int8_t *s) {
  printf("%s", s);
}

// input functions

int32_t tony_geti() {
  int32_t n;
  scanf("%" SCNd32, &n);
  return n;
}

int8_t tony_getb() {
  int8_t b;
  scanf("%" SCNd8, &b);
  return b;
}

int8_t tony_getc() {
  int8_t c;
  c = getchar();
  return c;
}

void tony_gets(int32_t n, int8_t *s) {
  fgets((char *)s, n, stdin);
}

// conversion functions

int32_t tony_abs(int32_t n) {
  if (n < 0)
    return -n;
  return n;
}

int32_t tony_ord(int8_t c) {
  // don't sign extend
  return (int32_t)((uint8_t)c);
}

int8_t tony_chr(int32_t n) {
  return (int8_t)n;
}

// string manipulation functions

int32_t tony_strlen(int8_t *s) {
  int32_t length = 0;

  while (*(s++) != 0)
    ++length;

  return length;
}

int32_t tony_strcmp(int8_t *s1, int8_t *s2) {
  while (1) {
    // if this condition is true, it means that all characters so far (if any) were equal
    // and we have reached the end of both strings
    if (*s1 == 0 && *s2 == 0)
      return 0;

    // s1 is smaller than s2 (shorter strings are lexicographically smaller)
    if (*s1 == 0)
      return -1;

    // s1 is greater than s2
    if (*s2 == 0)
      return 1;

    // found the first differing character, return based on which character is greater
    if (*s1 != *s2)
      return (*s1 > *s2) ? 1 : -1;

    ++s1;
    ++s2;
  }
}

void tony_strcpy(int8_t *trg, int8_t *src) {
  while (*src != 0) {
    *trg = *src;
    ++trg;
    ++src;
  }
  *trg = 0;
}

void tony_strcat(int8_t *trg, int8_t *src) {
  while (*trg != 0)
    ++trg;

  while (*src != 0) {
    *trg = *src;
    ++trg;
    ++src;
  }
  *trg = 0;
}
