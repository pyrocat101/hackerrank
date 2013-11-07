#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

const int MOD = 1000000000;
const int MAXC = 1000;
long long c[MAXC + 1][MAXC + 1];
void init() {
    for (int i = 0; i <= MAXC; i++) {
        for (int j = c[i][0] = 1; j <= i; j++) {
            c[i][j] = (c[i - 1][j] + c[i - 1][j - 1]) % MOD;
        }
    }
}

int main() {
    init();
    int cas;
    scanf("%d", &cas);
    while (cas--) {
        int x;
        scanf("%d", &x);
        cout << 1;
        for (int i = 1; i <= x; i++) {
            cout << " " << c[x][i];
        }
        cout << endl;
    }
    return 0;
}
