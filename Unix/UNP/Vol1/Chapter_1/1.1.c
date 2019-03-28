#include <unp/unp.h>


int main(int argc, char *argv[]){
    int sockfd, n;
    char recvline[MAXLINE + 1];
    struct sockaddr_in servaddr;

    if(argc != 2){
        err_quit("usage: a.out <IPaddress>");
    }

    // create a socket discriptor for AF_INET and SOCKSTREAM
    // socket is a part of SOCKET API
    if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0){
        err_sys("socket error");
    }

    // set the context of the servaddr to zero
    bzero(&servaddr, sizeof(servaddr));
    // set the addr_familt AF_INET and the port number to 13
    servaddr.sin_family = AF_INET;
    // the fuction htons set the host to net short number
    servaddr.sin_port = htons(13);

    // inet_ptor convert the ip address to proper format
    if(inet_pton(AF_INET, argv[1], &servaddr.sin_addr) <= 0){
        err_quit("inet_pton error for %s", argv[1]);
    }

    // establish a socket connect with the server specified by the struct servaddr
    // in unp we define SA to struct sockaddr
    if(connect(sockfd, (SA *) &servaddr, sizeof(servaddr)) < 0){
        err_sys("connect error");
    }

    // when we use TCP, we should use loop to read date from the TPC stream
    // if read == 0, break the loop
    while((n = read(sockfd, recvline, MAXLINE)) > 0){
        recvline[n] = 0;
        if(fputs(recvline, stdout) == EOF){
            err_sys("fputs error");
        }
    }

    if(n < 0){
        err_sys("read error");
    }

    // terminate the code and close all the open descriptor
    exit(0);
}
