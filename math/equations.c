#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define MOD  1000007
#define MAXN 1000000

int primes[MAXN + 1];

void gen_primes(void) {
  for (int i = 0; i <= MAXN; i++) {
    primes[i] = 1;
  }
  primes[0] = primes[1] = 0;
  for (int i = 2; i * i <= MAXN; i++) {
    if (primes[i] == 0) continue;
    for (int j = 2; j * i <= MAXN; j++) {
      primes[i * j] = 0;
    }
  }
}

void happy(int N) {
  long long sum = 1;
  for (int x = 2; x <= N; x++) {
    if (primes[x]) {
      int y = 0;
      for (int i = 1; ; i++) {
        int z = N / (long long)pow(x, i);
        if (z == 0) break;
        y += z;
      }
      sum = sum * (y * 2 + 1) % MOD;
    }
  }
  printf("%lld\n", sum);
}

int main() {
  int N;
  gen_primes();
  scanf("%d", &N);
  happy(N);
  return 0;
}
