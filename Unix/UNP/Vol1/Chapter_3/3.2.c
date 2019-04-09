int inet_pton(int family, const char *strptr, void *addrptr){
    if(family == AF_INET){
        // define a temp val
        struct in_addr in_val;

        if(inet_aton(strptr, &in_val)){
            // memory copy from in_val to addrptr
            memcpy(addrptr, &in_val, sizeof(struct in_addr));
            // return TRUE
            return (1);
        }
        // the strptr is valid but inet_aton has some error
        return (0);
    }
    // set the errno to EAFNOSUPPORT and return FALSE
    errno = EAFNOSUPPORT;
    return (-1);
}


const char* inet_ntop(int family, const void *addrptr, char *strptr, size_t len){
    // conver a number to a point to char and then use them as a array
    const u_char *p = (const u_char *) addrptr;

    // if is ipv4
    if(family == AF_INET){
        char temp[INET_ADDRSTRLEN];

        snprintf(temp, sizeof(temp), "%d.%d.%d.%d", p[0], p[1], p[2], p[3]);

        if(strlen(temp) >= len){
            errno = ENOSPC;
            return (NULL);
        }

        strcpy(strptr, temp);
        return (strptr);
    }

    errno = EAFNOSUPPORT;
    return (NULL);
}
