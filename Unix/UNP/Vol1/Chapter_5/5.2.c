#include <unp/unp.h>

void str_echo(int sockfd){
    ssize_t n;
    char buf[MAXLINE];

again:
    // read context from sockfd and write it to the client
    while((n = read(sockfd, buf, MAXLINE)) > 0){
        Writen(sockfd, buf, n);
    }

    if(n < 0 && errno == EINTR){
        goto again;
    }else if(n < 0){
        err_sys("str_echo: read error");
    }
}
