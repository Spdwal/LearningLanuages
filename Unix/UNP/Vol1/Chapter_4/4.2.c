#include <unp/unp.h>

int sockfd_to_family(int sockfd){
    struct sockaddr_storage ss;

    socklen_t len;

    len = sizeof(ss);

    if(getsockname(sockfd, (SA *) &ss, &len) < 0){
        return (-1);
    }

    return (ss.ss_family);
}
