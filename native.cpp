#include <cstdio>

extern "C" {

    void printInt(long long val) {
        printf("%lld\n", val);
    }

    void printDouble(double val) {
        printf("%lf\n", val);
    }

}