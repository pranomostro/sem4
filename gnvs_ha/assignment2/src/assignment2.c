#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "arguments.h"
#include "raw.h"
#include "hexdump.h"
#include "checksums.h"

/*====================================TODO===================================*/
/* Put your required struct definitions */

uint8_t mymac[ETH_ALEN];

typedef struct Frametype {
	uint16_t frametype;
	int frames, bytes;
} Frametype;

/* Put your ancillary functions here*/

int ftcmp(const void* a1, const void* a2)
{
	Frametype* f1=(Frametype *)a1;
	Frametype* f2=(Frametype *)a2;
	if(f1->frametype==f2->frametype)
		return 0;
	if(f1->frametype>f2->frametype)
		return 1;
	return -1;
}

/*===========================================================================*/

void assignment2(int fd, int frames)
{
	uint16_t ethtype;
	unsigned int timeout = 10000, i, forme=0, multicast=0;
	float total=0, ipv4=0, ipv6=0;
	uint8_t recbuffer[1514];
	size_t ret, ftsize=0;
	Frametype fts[255]; /* TODO: Make this variable length if it needs to */

	/* If you want to set up any data/counters before the receive loop,
	 * this is the right location
	 */

	memcpy(&mymac, grnvs_get_hwaddr(fd), ETH_ALEN);

	/* This is the ready marker! do not remove! */
	fprintf(stdout, "I am ready!\n");

	/* Update the loop condition */

	while(frames-->0) {
		ret = grnvs_read(fd, recbuffer, sizeof(recbuffer), &timeout);
		if (ret == 0) {
			fprintf(stderr, "Timed out, this means there was nothing to receive. Do you have a sender set up?\n");
			break;
		}

		/* This is the receive loop, 'recbuffer' will contain the received
		 * frame. 'ret' tells you the length of what you received.
		 * Anything that should be done with every frame that's received
		 * should be done here.
		 */

		ethtype=recbuffer[12]<<8|recbuffer[13];

		for(i=0; i<ftsize; i++)
			if(ethtype==fts[i].frametype)
				break;

		if(i==ftsize||ftsize==0)
		{
			ftsize++;
			fts[i].frametype=ethtype;
			fts[i].frames=1;
			fts[i].bytes=ret;
		}
		else
		{
			fts[i].frames++;
			fts[i].bytes+=ret;
		}

		if(recbuffer[0]&1)
			multicast++;
		else if(!memcmp(mymac, recbuffer, ETH_ALEN))
			forme++;

		if(ethtype==0x0800)
			ipv4+=ret;
		else if(ethtype==0x86DD)
			ipv6+=ret;
		total+=ret;
	}

	qsort(fts, ftsize, sizeof(Frametype), ftcmp);

	/* Print your summary here */
	for(i=0; i<ftsize; i++)
		printf("0x%.04x: %d frames, %d bytes\n", fts[i].frametype, fts[i].frames, fts[i].bytes);
	printf("%d of them were for me\n", forme);
	printf("%d of them were multicast\n", multicast);
	printf("IPv4 accounted for %.1f%% and IPv6 for %.1f%% of the traffic\n", 100*ipv4/total, 100*ipv6/total);
}

int main(int argc, char ** argv)
{
	struct arguments args;
	int sock;

	setvbuf(stdout, NULL, _IOLBF, 0);

	if ( parse_args(&args, argc, argv) < 0 ) {
		fprintf(stderr, "Failed to parse arguments, call with "
			"--help for more information\n");
		return -1;
	}

	if ( (sock = grnvs_open(args.interface, SOCK_RAW)) < 0 ) {
		fprintf(stderr, "grnvs_open() failed: %s\n", strerror(errno));
		return -1;
	}

	assignment2(sock, args.frames);

	grnvs_close(sock);

	return 0;
}
