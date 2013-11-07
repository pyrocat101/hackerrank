#include <map>
#include <set>
#include <list>
#include <cmath>
#include <ctime>
#include <deque>
#include <queue>
#include <stack>
#include <bitset>
#include <cstdio>
#include <limits>
#include <vector>
#include <cstdlib>
#include <numeric>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cstring>
using namespace std;
/* Head ends here */

long long stringsimilarity(char* string) {
    long len = strlen(string);
    long long sim = len;
    for (int i = 1; i < len; i++) {
        int s = 0;
        for (int j = i, k = 0; j < len; j++, k++) {
            if (string[j] == string[k]) {
                s++;
            } else {
                break;
            }
        }
        sim += s;
    }
    return sim;
}

int main() {
    long long res, t, i;
    scanf("%d",&t);
    char a[100001];
    for (i=0;i<t;i++) {
        scanf("%s",a);
        res=stringsimilarity(a);
        printf("%lld\n",res);
    }

    /* Enter your code here. Read input from STDIN. Print output to STDOUT */
    return 0;
}
