#include<apue.h>

int main(){
    int c;
    while((c = getc(stdin)) != EOF){
        if(putc(c, stdout) == EOF){
            err_sys("Output error.\n");
        }
    }

    if(ferror(stdin)){
        err_sys("Input error.\n");
    }

    return 0;
}
