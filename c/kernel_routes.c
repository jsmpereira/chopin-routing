#include <errno.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <unistd.h>

#define BUFFER_SIZE 8192

extern int modify_route(char* destination, char* gateway, char* interface, int metric, int set)
{
  struct {
    struct nlmsghdr nl;
    struct rtmsg rt;
    char buf[BUFFER_SIZE];
  } req; // RTNETLINK request

    int sock = -1;
    struct sockaddr_nl addr;
    struct msghdr msg;
    struct sockaddr_nl pa;
    struct iovec iov;

    unsigned int if_idx = if_nametoindex(interface);

    int rtn;
    int rtl;
    struct rtattr *rtap;

    /* Setup socket */
    bzero (&addr, sizeof(addr));

    if ((sock = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE)) < 0)
        perror("socket");

    addr.nl_family = AF_NETLINK;
    addr.nl_groups = RTMGRP_IPV4_ROUTE;

    if (bind(sock,(struct sockaddr *)&addr,sizeof(addr)) < 0)
        perror("bind");

    // build request
    bzero(&req, sizeof(req)); // init request buffer
    rtl = sizeof(struct rtmsg);

    // Add attributes
    // First: destination IP`
    rtap = (struct rtattr *) req.buf;
    rtap->rta_type = RTA_DST;
    rtap->rta_len = sizeof(struct rtattr) + 4;
    inet_pton(AF_INET, destination, ((char *) rtap) + sizeof(struct rtattr));
    rtl += rtap->rta_len;

    // Second: interface index
    rtap = (struct rtattr *) (((char *) rtap) + rtap->rta_len);
    rtap->rta_type = RTA_OIF;
    rtap->rta_len = sizeof(struct rtattr) + 4;
    memcpy(((char *) rtap) + sizeof(struct rtattr), &if_idx, 4);
    rtl += rtap->rta_len;

    // Third: set metric
    rtap = (struct rtattr *) (((char *) rtap) + rtap->rta_len);
    rtap->rta_type = RTA_PRIORITY;
    rtap->rta_len = sizeof(struct rtattr) + 4;
    memcpy(((char *) rtap) + sizeof(struct rtattr), &metric, 4);
    rtl += rtap->rta_len;

    // If present, add Gateway
    if (gateway) {
      rtap = (struct rtattr *) (((char *) rtap) + rtap->rta_len);
      rtap->rta_type = RTA_GATEWAY;
      rtap->rta_len = sizeof(struct rtattr) + 4;
      inet_pton(AF_INET, gateway, ((char *) rtap) + sizeof(struct rtattr));
      rtl += rtap->rta_len;
    }

    req.nl.nlmsg_len = NLMSG_LENGTH(rtl);
    req.nl.nlmsg_flags = NLM_F_REQUEST;

    if (set)
      req.nl.nlmsg_flags |= NLM_F_CREATE | NLM_F_REPLACE;

    req.nl.nlmsg_type = set ? RTM_NEWROUTE : RTM_DELROUTE;

    req.rt.rtm_family = AF_INET;
    req.rt.rtm_table = RT_TABLE_MAIN;
    req.rt.rtm_protocol = RTPROT_STATIC;
    req.rt.rtm_scope = RT_SCOPE_UNIVERSE;
    req.rt.rtm_type = RTN_UNICAST;
    req.rt.rtm_dst_len = 32;

    // send request to kernel
    bzero(&pa, sizeof(pa));
    pa.nl_family = AF_NETLINK;
    
    bzero(&msg, sizeof(msg));
    msg.msg_name = (void *) &pa;
    msg.msg_namelen = sizeof(pa);

    iov.iov_base = (void *) &req.nl;
    iov.iov_len = req.nl.nlmsg_len;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    
    rtn = sendmsg(sock, &msg, 0);

    if((rtn = sendmsg(sock, &msg, 0)) < 0){
      fprintf(stderr, "ERROR: %s\n", strerror(errno));
      exit(1);
    }

    /* Close socket */
    close(sock);

    return 0;
}


int main(int argc, char **argv) {

  if (argc < 5) {
    printf("Usage: modify_route <dst> <gw> <if> <metric> <op>\n");
    exit(0);
  }

  modify_route(argv[1], argv[2], argv[3], atoi(argv[4]), atoi(argv[5]));

  return 0;
}
