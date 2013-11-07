#include <iostream>
#include <cstdio>
#include <cmath>

#define MOD 1000000007
#define MAX 1111

using namespace std;

long long row[MAX];
long long solid[MAX];

long long mod_pow(long long base, long long exp) {
  long long ret = 1;
  while (exp > 0) {
    if (exp % 2 == 1) {
      ret = (ret * base) % MOD;
    }
    exp /= 2;
    base = (base * base) % MOD;
  }
  return ret;
}

void happy(int n, int m) {
  for (int i = 0; i < 4 && i < m; i++) {
    row[i] = pow(2, i);
  }
  for (int i = 4; i < m; i++) {
    row[i] = 0;
    row[i] = (row[i] + row[i - 1]) % MOD;
    row[i] = (row[i] + row[i - 2]) % MOD;
    row[i] = (row[i] + row[i - 3]) % MOD;
    row[i] = (row[i] + row[i - 4]) % MOD;
  }

  // all walls
  for (int i = 0; i < m; i++) {
    row[i] = mod_pow(row[i], n);
  }

  // solid walls
  for (int i = 0; i < m; i++) {
    solid[i] = row[i];
    for (int j = 1; j <= i; j++) {
      long long tmp = (row[j - 1] * solid[i - j]) % MOD;
      // solid[i] -= (row[j - 1] * solid[i - j]) % MOD;
      solid[i] = (MOD + solid[i] - tmp) % MOD;
    }
  }

  cout << solid[m - 1] % MOD << endl;
}

int main() {
  int cas;
  scanf("%d", &cas);
  while (cas--) {
    int n, m;
    scanf("%d %d", &n, &m);
    happy(n, m);
  }
}
