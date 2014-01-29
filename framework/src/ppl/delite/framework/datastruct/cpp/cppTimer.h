#include <iostream>
#include <string>
using namespace std;

#include <sys/time.h>

class cppTimer {
    timespec start;
    timespec end;

    long getTime()
    {
        timespec ts;
        if (0 != clock_gettime(CLOCK_REALTIME, &ts)) {
            cerr << "Something wrong happened in getTime" << endl;
        }
        return ts;
    }
    
public:
    cppTimer()
    {
        end = {0,0};
        start = getTime();
    }

    void reset()
    {
        end = {0,0};
        start = getTime();
    }

    long getElapsedNs()
    {
        end = getTime();
        return end.tv_nsec - start.tv_nsec; // TODO: account for overflows
    }

    double getElapsedS()
    {
        end = getTime();
        return end.tv_sec - start.tv_sec; // TODO: account for overflows
    }
};



/*
void kernel_1()
{
    // header
    cppTimer T;

    for(int i = 0; i< 10000; i++) {
        ;
    }

    // footer
    cout << "Time taken: " << T.getElapsedNs() << "ns" << endl;
    return;
}

void kernel_2()
{
    // header
    cppTimer T;

    for(int i = 0; i< 1000; i++) {
        ;
    }

    // footer
    cout << "Time taken: " << T.getElapsedNs() << "ns" << endl;
    return;
}

void kernel_3()
{
    // header
    cppTimer T;
    for(int i = 0; i< 100000; i++) {
        ;
    }

    // footer
    cout << "Time taken: " << T.getElapsedNs() << "ns" << endl;
    return;
}

int main()
{

//    std::string str ("Please split this sentence into tokens");
//    char * cstr = new char [str.length()+1];
//    std::strcpy (cstr, str.c_str());

//    char *p = std::strtok(cstr, " ");
//    while (p != 0) {

//    }

    kernel_1();
    kernel_2();
    kernel_3();
    return 0;
}
*/
