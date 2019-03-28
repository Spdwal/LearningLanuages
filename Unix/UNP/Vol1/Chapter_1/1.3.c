#include <unp/unp.h>
#include <time.h>

int main(int argc, char *argv[]){
    int listenfd, connfd;
    struct sockaddr_in servaddr;
    char buff[MAXLINE];
    time_t ticks;

    // create a TCP socket to the client
    listenfd = Socket(AF_INET, SOCK_STREAM, 0);


    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(13);
    // The server's port is bound to the socket struct servaddr
    Bind(listenfd, (SA*) &servaddr, sizeof(servaddr));
    // The socket is coverted into a listening socket
    // socket bind listen are the nomal steps for any TCP server
    // to prepare what we call the listenting descriptor
    listen(listenfd, LISTENQ);

    // The server process is put to sleep in the call to accept
    // waiting for a client connection to arrive and be accepted
    // A TCP connection uses a three-way handshake to establish a connection
    // When the handshake completes accept returns
    // and the return value is a connected descriptor
    // In a infinite loop
    for(;;){
        connfd= Accept(listenfd, (SA *) NULL, NULL);
        ticks = time(NULL);
        // snprintf can check the overflow of the destination buffer
        snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks));
        // write the time string to connect descriptor
        Write(connfd, buff, strlen(buff));

        Close(connfd);
    }
}
