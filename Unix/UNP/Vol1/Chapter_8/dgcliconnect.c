#include <unp/unp.h>

void dg_cli(FILE *fp, int sockfd, const SA *pservaddr, socklen_t servlen){
    int n;
    char sendline[MAXLINE], recvline[MAXLINE + 1];
    // use connect to the udp socket
    // so we have replece sendto and recvfrom to write and read
    Connect(sockfd, (SA *) pservaddr, servlen);

    while(Fgets(sendline, MAXLINE, fp) != NULL){
        Write(sockfd, sendline, strlen(sendline));

        n = Read(sockfd, recvline, MAXLINE);

        recvline[n] = 0;
        Fputs(recvline, stdout);
    }
}
