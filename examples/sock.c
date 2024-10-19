// Server side C program to demonstrate Socket
// programming
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

struct sockaddr_in address;
socklen_t addrlen;

void sock_close(int fd) {
    close(fd);
}

void sock_write(int fd, const char* msg, int size) {
    send(fd, msg, size, 0);
}

void sock_listen(int fd, int backlog) {
    if (listen(fd, backlog) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
    puts("Socket listen");
}

int sock_accept(int fd) {
    int new_socket = accept(fd, (struct sockaddr*)&address, &addrlen);
    if (new_socket < 0) {
        perror("accept");
        exit(EXIT_FAILURE);
    }
    puts("New socket connection accepted");
    return new_socket;
}

int sock(int port) {
    int opt = 1;
    addrlen = sizeof(address);
    char buffer[1024] = { 0 };

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }
    puts("Socket open");

    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt))) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }
    puts("Socket bound");
    return server_fd;
}