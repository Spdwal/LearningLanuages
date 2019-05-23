#include <unp/unp.h>



// the function will run forever
// it is a iterative server
void dg_echo(int sockfd, SA *pcliaddr, socklen_t clilen){
    int n;
    socklen_t len;
    char mesg[MAXLINE];


    for(;;){
        len = clilen;
        // get the address of the client addr and save it to pcliaddr
        n = Recvfrom(sockfd, mesg, MAXLINE, 0, pcliaddr, &len);

        Sendto(sockfd, mesg, n, 0, pcliaddr, len);

    }
}
