#include <unp/unp.h>

void str_cli(FILE *fp, int sockfd){
    int maxfdpl;
    fd_set rset;
    char sendline[MAXLINE], recvline[MAXLINE];

    FD_ZERO(&rset);

    for(;;){
        // set a decriptor set to readable or
        FD_SET(fileno(fp), &rset);
        FD_SET(sockfd, &rset);
        // fileno convert a standard IO file pointer into its corresponding descriptor
        maxfdpl = max(fileno(fp), sockfd) + 1;
        // block something is ready
        Select(maxfdpl, &rset, NULL, NULL, NULL);

        // if sockfd is readable the echo line is read from sockfd and output by fputs
        if(FD_ISSET(sockfd, &rset)){
            if(Readline(sockfd, recvline, MAXLINE) == 0){
                err_quit("strcli: server terminated prematurely");
            }
            Fputs(recvline, stdout);
        }


        // if the standard input is readable a line is read by fgets and written to the socket by using written.
        if(FD_ISSET(fileno(fp), &rset)){
            if(Fgets(sendline, MAXLINE, fp) == NULL){
                return;
            }

            Writen(sockfd, sendline, strlen(sendline));
        }
    }
}
