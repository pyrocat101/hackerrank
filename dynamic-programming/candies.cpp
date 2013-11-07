#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

#define MAX 100009

int N[MAX];
long long DPL[MAX];
long long DPR[MAX];
long long DP[MAX];

void happy(int nchild) {

    DPL[1] = 1;
    DPR[nchild] = 1;

    for (int i = 2; i <= nchild; i++) {
        if (N[i - 1] < N[i]) {
            DPL[i] = DPL[i - 1] + 1;
        } else {
            DPL[i] = 1;
        }
    }

    for (int i = nchild - 1; i >= 1; i--) {
        if (N[i] > N[i + 1]) {
            DPR[i] = DPR[i + 1] + 1;
        } else {
            DPR[i] = 1;
        }
    }

    for (int i = 1; i <= nchild; i++) {
        DP[i] = max(DPL[i], DPR[i]);
    }

    long long sum = accumulate(DP + 1, DP + nchild + 1, 0);

    cout << sum << endl;
}

int main() {
    int cas, nchild;
    scanf("%d", &cas);
    nchild = cas;
    for (int i = 0; i < cas; i++) {
        scanf("%d", &N[i + 1]);
    }
    happy(nchild);
    return 0;
}
