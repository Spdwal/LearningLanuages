#include <unp/unp.h>

#define NDG 2000
#define DGLEN 1400


void dg_cli(FILE *fp, int sockfd, const SA *pservaddr, socklen_t servlen){
    int i;
    char sendline[DGLEN];


    // send 2000 times 1400 bytes to udp server
    for(i = 0; i < NDG; ++i){
        Sendto(sockfd, sendline, DGLEN, 0, pservaddr, servlen);
    }
}
