#include <unp/unp.h>

void sig_chld(int signo){
    pid_t pid;
    int stat;
    // the wait fuction suspends exectution of its calling process until stat imformation is available for a terminated child process or a signal is received
    // the stat contains termination information about the process that exited
    pid = wait(&stat);
    printf("child %d terminated\n", pid);
}
