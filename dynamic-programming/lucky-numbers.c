#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MAG 18
#define MAX_SUM 162
#define MAX_SQ_SUM 1458

int primes[1460];
long long dp[19][164][1460];
// lucky number count till {current bit, current bit value, sum, sq_sum}
long long ans[19][10][164][1460];
int sq_sum_start[19][164];
int sq_sum_end[19][164];

void gen_primes(void) {
  for (int i = 0; i <= MAX_SQ_SUM; i++) {
    primes[i] = 1;
  }
  primes[0] = primes[1] = 0;
  for (int i = 2; i * i <= MAX_SQ_SUM; i++) {
    if (primes[i] == 0) continue;
    for (int j = 2; j * i <= MAX_SQ_SUM; j++) {
      primes[i * j] = 0;
    }
  }
}

void gen_dp(void) {
  memset(dp, 0, sizeof(dp));
  dp[0][0][0] = 1;
  for (int i = 0; i < MAX_MAG; i++) {
    for (int j = 0; j <= 9 * i; j++) {
      for (int k = 0; k <= 9 * 9 * i; k++) {
        for (int l = 0; l < 10; l++) {
          dp[i + 1][j + l][k + l * l] += dp[i][j][k];
        }
      }
    }
  }
}

long long feeling_lucky(long long upper) {
  long long result = 0;
  int len = 0;
  int bit_max[MAX_MAG];
  while (upper) {
    bit_max[len] = upper % 10;
    upper /= 10;
    len++;
  }
  // sum: sum of `upper` bits till `current bit`
  int sum = 0;
  // sq_sum: square sum of `upper` bits till `current bit`
  int sq_sum = 0;
  long long bit_result;
  long long bit_val_result;
  // most significant bit to least significant bit
  for (int i = len - 1; i >= 0; i--) {
    bit_result = 0;
    // bit result <- lucky count of {[0 ~ (current bit - 1)] 00...0 ~ 99...9}
    for (int l = 0; l < bit_max[i]; l++) {
      bit_val_result = 0;
      if (ans[i][l][sum][sq_sum]) {
        // answer is cached
        bit_result += ans[i][l][sum][sq_sum];
        continue;
      }
      // x: sum of bits including current bits `l`
      // y: square sum of bits including current bits `l`
      int x = l + sum, y = l * l + sq_sum;
      // 00...0 ~ 99...9
      for (int j = 0; j <= i * 9; j++) {
        // j: sum of bits
        if (primes[j + x]) {
          for (int k = sq_sum_start[i][j]; k <= sq_sum_end[i][j]; k++) {
            if (primes[k + y]) {
              bit_result += dp[i][j][k];
              bit_val_result += dp[i][j][k];
            }
          }
        }
        // for (int k = 0; k <= i * 9 * 9; k++) {
        //   // k: square sum of bits
        //   if (primes[j + x] && primes[k + y]) {
        //     bit_result += dp[i][j][k];
        //     bit_val_result += dp[i][j][k];
        //   }
        // }
      }
      ans[i][l][sum][sq_sum] = bit_val_result;
    }
    result += bit_result;
    sum += bit_max[i];
    sq_sum += bit_max[i] * bit_max[i];
  }

  // at last, `upper` itself
  if (primes[sum] && primes[sq_sum]) {
    ++result;
  }
  return result;
}

int main(void) {
  gen_primes();
  gen_dp();

  for (int i = 0; i <= MAX_MAG; i++) {
    for (int j = 0; j <= MAX_SUM; j++) {
      for (int k = 0; k <= MAX_SQ_SUM; k++) {
        if (dp[i][j][k]) {
          sq_sum_start[i][j] = k;
          break;
        }
      }
      for (int k = MAX_SQ_SUM; k >= 0; k--) {
        if (dp[i][j][k]) {
          sq_sum_end[i][j] = k;
          break;
        }
      }
    }
  }

  int cas;
  scanf("%d", &cas);
  while (cas--) {
    long long a, b;
    scanf("%lld %lld", &a, &b);
    printf("%lld\n", feeling_lucky(b) - feeling_lucky(a - 1));
  }
  return 0;
}
