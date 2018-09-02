#include<apue.h>

int main(){
    printf("Hello world from process ID %ld\n", (long)getpid());
    return 0;
}
