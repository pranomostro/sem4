#include <netinet/ether.h>
#include <net/ethernet.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <stdlib.h>
#include <netinet/ip6.h>
#include <netinet/icmp6.h>
#include <asm/byteorder.h>

#include "traceroute.h"
#include "raw.h"
#include "hexdump.h"
#include "checksums.h"

/*
 * We do not use the kernel's definition of the IPv6 header (struct ipv6hdr)
 * because the definition there is slightly different from what we would expect
 * (the problem is the 20bit flow label - 20bit is brain-damaged).
 *
 * Instead, we provide you struct that directly maps to the RFCs and lecture
 * slides below.
 */

struct ipv6_hdr {
#if defined(__LITTLE_ENDIAN_BITFIELD)
	uint32_t tc1:4, version:4, flow_label1:4, tc2:4, flow_label2:16;
#elif defined(__BIG_ENDIAN_BITFIELD)
	uint32_t version:4, tc1:4, tc2:4, flow_label1:4, flow_label2:16;
#else
#error "You did something wrong"
#endif
	uint16_t plen;
	uint8_t nxt;
	uint8_t hlim;
	struct in6_addr src;
	struct in6_addr dst;
} __attribute__((packed));


/*====================================TODO===================================*/

/* Note: It can be extremely helpful to create some structures to access the
 * fields of the packets received.
 *
 * A look at the C headers in /usr/include/net and /usr/include/netinet pays
 * off
 */


/*===========================================================================*/


/**
 * This is the entry point for student code.
 * We do highly recommend splitting it up into multiple functions.
 *
 * A good rule of thumb is to make loop bodies functions and group operations
 * that work on the same layer into functions.
 *
 * For reading from the network have a look at assignment2. Also read the
 * comments in libraw/include/raw.h
 *
 * To get your own IP address use the grnvs_get_ip6addr function.
 * This one is also documented in libraw/include/raw.h
 */
void run(int fd, const char *ipaddr, int timeoutval, int attempts,
         int hoplimit)
{
	char ipname[INET6_ADDRSTRLEN];
	struct in6_addr dstip;
	struct in6_addr srcip;
	uint8_t packet[1514];
	size_t length;
	int seq;
	ssize_t ret;


	/* Cast everything to void, this exists to silence warnings in the
	 * template.
	 * Feel free to remove this while you work.
	 */
	(void) dstip; (void) srcip; (void) packet; (void) length; (void) seq;
	(void) ipname; (void) fd; (void) timeoutval; (void) attempts;
	(void) hoplimit; (void) ipaddr;

/*====================================TODO===================================*/
	/*
	 * TODO:
	 * 1) Initialize the addresses required to build the packet.
	 * 2) Loop over hoplimit and attempts
	 * 3) Build and send a packet for each iteration
	 * 4) Print the hops found in the specified format
	 */

	while (0) {
		uint16_t cksum;

		/* Make sure you set your length before you get here */
		cksum = icmp6_checksum((struct ip6_hdr *) packet, packet + 40,
		                       length - 40);
		/* You still have to put the checksum into the buffer! */
		(void) cksum;

		if (( ret = grnvs_write(fd, packet, length)) < 0 ) {
			fprintf(stderr, "grnvs_write() failed: %ld\n", ret);
			hexdump(packet, length);
			exit(-1);

		};
	}

/*===========================================================================*/
}

int main(int argc, char ** argv)
{
	struct arguments args;
	int sock;

	if ( parse_args(&args, argc, argv) < 0 ) {
		fprintf(stderr, "Failed to parse arguments, call with "
			"--help for more information\n");
		return -1;
	}

	if ( (sock = grnvs_open(args.interface, SOCK_DGRAM)) < 0 ) {
		fprintf(stderr, "grnvs_open() failed: %s\n", strerror(errno));
		return -1;
	}

	setbuf(stdout, NULL);

	run(sock, args.dst, args.timeout, args.attempts, args.hoplimit);

	grnvs_close(sock);

	return 0;
}
