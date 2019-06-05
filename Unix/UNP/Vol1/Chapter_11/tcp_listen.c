#include <unp/unp.h>

int tcp_listen(const char *host, const char *serv, socklen_t *addrlenp){
    int listenfd, n;
    const int on = 1;
    struct addrinfo hints, *res, *ressave;

    bzero(&hints, sizeof(struct addrinfo));
    hints.ai_flags = AI_PASSIVE;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if((n = getaddrinfo(host, serv, &hints, &res)) != 0){
        err_quit("tcp_listen error for %s, %s: %s", host, serv, gai_strerror(n));
    }

    ressave = res;

    do{
        listenfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

        if(listenfd < 0){
            continue;
        }
        // for a tcp server, we always set SO_REUSEADDR flag
        Setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
        if(bind(listenfd, res->ai_addr, res->ai_addrlen) == 0)
            break;

        Close(listenfd);
    } while((res = res->ai_next) != NULL);

    if(res == NULL){
        err_sys("tcp_listen error for %s, %s", host, serv);
    }

    Listen(listenfd, LISTENQ);

    // return the size of protocol
    if(addrlenp){
        *addrlenp = res->ai_addrlen;
    }

    freeaddrinfo(ressave);

    return listenfd;
}
