#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define MOD 1000000007
#define MAXSTR 100000

char a[MAXSTR + 1], b[MAXSTR + 1];

long long mod_pow(long long a, long long b) {
  long long ret = 1;
  while (b) {
    if (b & 1) {
      ret = ret * a % MOD;
    }
    b >>= 1;
    a = a * a % MOD;
  }
  return ret;
}

void happy(char *a, char *b) {
  int a_len = strlen(a);
  int b_len = strlen(b);
  long long a_mod = a[0] - '0';
  for (int i = 1; i < a_len; i++) {
    a_mod = (a_mod * 10 + (a[i] - '0')) % MOD;
  }

  long long b_mod = b[0] - '0';
  for (int i = 1; i < b_len; i++) {
    b_mod = (b_mod * 10 + (b[i] - '0')) % (MOD - 1);
  }

  printf("%lld\n", mod_pow(a_mod, b_mod));
}

int main() {
  int cas;
  scanf("%d\n", &cas);
  while (cas--) {
    scanf("%s %s", a, b);
    happy(a, b);
  }
  return 0;
}
