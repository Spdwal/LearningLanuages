#include <unp/unp.h>

void str_cli(FILE *fp, int sockfd){

    // the stdineof is a flag.
    // if the flag == 0, we select on stdio readable.
    int    maxfdpl, stdineof;
    fd_set rset;
    char   buf[MAXLINE];
    int    n;

    stdineof = 0;
    FD_ZERO(&rset);
    for(;;){
        if(stdineof == 0){
            FD_SET(fileno(fp), &rset);
        }

        FD_SET(sockfd, &rset);

        maxfdpl = max(fileno(fp), sockfd) + 1;

        Select(maxfdpl, &rset, NULL, NULL, NULL);

        // if read an EOF from sockfd but we don't read an EOF from stdio, the server terminated prematurely
        if(FD_ISSET(sockfd, &rset)){
            if((n = Read(sockfd, buf, MAXLINE)) == 0){
                if(stdineof == 1){
                    return ;
                }else{
                    err_quit("std_cli: server terminated prematurely");
                }
            }

            Write(fileno(stdout), buf, n);
        }

        if(FD_ISSET(fileno(fp), &rset)){
            // if we read a EOF from stdio we set the stdineof to 1
            if((n = Read(fileno(fp), buf, MAXLINE)) == 0){
                stdineof = 1;
                // send a FIN
                Shutdown(sockfd, SHUT_WR);
                FD_CLR(fileno(fp), &rset);
                continue;
            }

            Writen(sockfd, buf, n);
        }
    }


}
