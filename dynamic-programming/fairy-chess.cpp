#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cstring>
using namespace std;

class Imp {
public:
    static const int MOD = 1000000007;
    typedef long long LL;
    int u;
    Imp(int v = 0):u(v) {}
    operator int& () { return u; }
    operator const int& () const { return u; }
    Imp operator + (int v) const { return u + v < MOD ? u + v : u + v - MOD; }
    Imp operator - (int v) const { return u < v ? u - v + MOD : u - v; }
    Imp operator * (int v) const { return u * LL(v) % MOD; }
    Imp operator / (int v) const { return u * RR(v) % MOD; }
    Imp operator - () const { return u ? MOD - u : 0; }
    Imp& operator += (int v) { u += u + v < MOD ? v : v - MOD; return *this; }
    Imp& operator -= (int v) { u -= u < v ? v - MOD : v; return *this; }
    Imp& operator *= (int v) { u = u * LL(v) % MOD; return *this; }
    Imp& operator /= (int v) { u = u * RR(v) % MOD; return *this; }
    static LL RR(int x) {
        return x > 1 ? RR(MOD % x) * (MOD - MOD / x) % MOD : x;
    }
};

#define MAX 200

// rotated chess board
char board[MAX * 2][MAX * 2];
Imp dp[MAX][MAX * 2][MAX * 2];
// square area from (0, 0) to (i, j)
Imp areas[MAX * 2][MAX * 2];

void happy(int N, int M, int S) {

  memset(dp, 0, sizeof(dp));
  memset(areas, 0, sizeof(areas));
  memset(board, 'P', sizeof(board));

  // read map
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      int ii = i;
      int jj = j;

      int x = ii - jj + N - 1;
      int y = ii + jj;
      char c = getchar();
      // init dp[0, x, y]
      if (c == 'L') {
        dp[0][x][y] = 1;
        // update areas
        for (int a = x; a < N * 2 - 1; a++) {
          for (int b = y; b < N * 2 - 1; b++) {
            areas[a][b] = 1;
          }
        }
      }
      board[x][y] = c;
    }
    // eat linefeed
    getchar();
  }

  for (int m = 1; m <= M; m++) {

    for (int i = 0; i < N * 2 - 1; i++) {
      for (int j = 0; j < N * 2 - 1; j++) {

        int xstart = max(0, i - S);
        int ystart = max(0, j - S);
        int xend = min(i + S, N * 2 - 2);
        int yend = min(j + S, N * 2 - 2);

        // cannot place on pawn
        if (board[i][j] == 'P') continue;

        // square area
        dp[m][i][j] = areas[xend][yend];
        if (xstart - 1 >= 0 && ystart - 1 >= 0) {
          dp[m][i][j] += areas[xstart - 1][ystart - 1];
        }
        if (xstart - 1 >= 0) {
          dp[m][i][j] -= areas[xstart - 1][yend];
        }
        if (ystart - 1 >= 0) {
          dp[m][i][j] -= areas[xend][ystart - 1];
        }
      }
    }

    // update areas
    areas[0][0] = dp[m][0][0];
    for (int i = 1; i < N * 2 - 1; i++) {
      // first row
      areas[0][i] = areas[0][i - 1] + dp[m][0][i];
      // first column
      areas[i][0] = areas[i - 1][0] + dp[m][i][0];
    }
    for (int i = 1; i < N * 2 - 1; i++) {
      for (int j = 1; j < N * 2 - 1; j++) {
        areas[i][j] = dp[m][i][j];
        areas[i][j] += areas[i - 1][j] + areas[i][j - 1] - areas[i - 1][j - 1];
      }
    }
  }

  // sum up
  Imp moves = 0;
  for (int i = 0; i < N * 2 - 1; i++) {
    for (int j = 0; j < N * 2 - 1; j++) {
      moves += dp[M][i][j];
    }
  }

  cout << moves.u << endl;
}

int main() {
    int cas;
    scanf("%d", &cas);
    while (cas--) {
      int N, M, S;
      scanf("%d %d %d\n", &N, &M, &S);
      happy(N, M, S);
    }

    return 0;
}
