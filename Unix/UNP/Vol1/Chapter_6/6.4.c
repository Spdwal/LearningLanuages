#include <unp/unp.h>
#include <limits.h>


int main(int argc, char *argv[]){
    int i, maxi, listenfd, connfd, sockfd;
    int nready;
    ssize_t n;
    char buf[MAXLINE];
    socklen_t clilen;
    struct pollfd client[OPEN_MAX];
    struct sockaddr_in cliaddr, servaddr;

    listenfd = Socket(AF_INET, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);

    Bind(listenfd, (SA *) &servaddr, sizeof(servaddr));

    Listen(listenfd, LISTENQ);

    client[0].fd = listenfd;
    client[0].events = POLLRDNORM;
    for(i = 1; i < OPEN_MAX; i++){
        client[i].fd = -1;
    }

    maxi = 0;
    for(;;){
        nready = Poll(client, maxi + 1, INFTIM);

        // new client connection
        if(client[0].revents & POLLRDNORM){
            clilen = sizeof(cliaddr);
            connfd = Accept(listenfd, (SA *) &cliaddr, &clilen);

            // save the connect descriptor
            for(i = 1; i < OPEN_MAX; i++){
                if(client[i].fd < 0){
                    client[i].fd = connfd;
                    break;
                }
            }

            if(i == OPEN_MAX){
                err_quit("too many clients");
            }

            client[i].events = POLLRDNORM;

            if(i > maxi){
                maxi = i;
            }

            if(--nready <= 0){
                continue;
            }
        }

        // check all clients for data
        for(i = 1; i <= maxi; i++){
            if((sockfd = client[i].fd) < 0){
                continue;
            }

            // the descriptor is readable or recieved a error
            if(client[i].revents & (POLLRDNORM | POLLERR)){
                if((n = read(sockfd, buf, MAXLINE)) < 0){
                    // the connection is reset by client
                    if(errno == ECONNRESET){
                        Close(sockfd);
                        client[i].fd = 01;
                    }else{
                        err_sys("read error");
                    }
                }else if(n == 0){
                    // the client is closed by client
                    Close(sockfd);
                    client[i].fd = -1;
                }else{
                    Writen(sockfd, buf, n);
                }

                if(--nready <= 0){
                    break;
                }
            }
        }
    }
}
