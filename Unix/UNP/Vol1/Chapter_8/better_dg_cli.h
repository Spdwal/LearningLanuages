#include <unp/unp.h>

void dg_cli(FILE *fp, int sockfd, const SA *pservaddr, socklen_t servlen){
    int n;
    char sendline[MAXLINE], recvline[MAXLINE + 1];
    socklen_t len;
    struct sockaddr *preply_addr;

    preply_addr = Malloc(servlen);

    while(Fgets(sendline, MAXLINE, fp) != NULL){
        Sendto(sockfd, sendline, strlen(sendline), 0, pservaddr, servlen);

        len = servlen;
        n = Recvfrom(sockfd, recvline, MAXLINE, 0, preply_addr, &len);


        // check the len returned by kernel.
        // compare the preply_addr and pservaddr
        // make sure that the two adresses are the same.
        if(len != servlen || memcmp(pservaddr, preply_addr, len) != 0){
            printf("reply from %s (ignored)\n", Sock_ntop(preply_addr, len));
            continue;
        }

        recvline[0] = 0;
        Fputs(recvline, stdout);
    }
}
