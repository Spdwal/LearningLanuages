//此程序只支持运行内建命令，但是不支持向命令传递参数。

#include<apue.h>
#include<sys/wait.h>

int main(){
    char  buf[MAXLINE];
    pid_t pid;
    int   status;

    printf("%% ");
    while(fgets(buf, MAXLINE, stdin) != NULL){
        if(buf[strlen(buf) - 1] == '\n'){
            buf[strlen(buf) - 1] = 0;
        }
        // fork函数返回两次，在父进程中返回子进程的ID，子进程中fork返回0.
        if((pid = fork()) < 0){                             //父进程内若fork返回值小于0，则fork失败。
            err_sys("fork error.");
        }else if (pid == 0){                                //子进程内fork函数返回值为0
            //fork后跟的exec函数就是某些操作系统所称的产生(spawn)一个新进程。
            execlp(buf, buf, (char *) 0);
            err_ret("Couldn't execute: %s", buf);
            exit(127);
        }

        //等待子进程结束，status存储子进程pid的终止状态。如果需要，可以用此值判断子进程是如何终止的。
        if((pid = waitpid(pid, &status, 0)) < 0){
            err_sys("waitpid");
        }
        printf("%% ");
    }

    exit(0);
}
