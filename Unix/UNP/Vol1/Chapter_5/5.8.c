#include <unp/unp.h>

void sig_chld(int signo){
    pid_t pid;
    int stat;

    // the first arg of waitpid means wait for every child process
    // the WNOHANG means don't hang when there is any child process is running
    while((pid= waitpid(-1, &stat, WNOHANG)) > 0){
        printf("child %d terminated\n", pid);
    }

    return;
}
