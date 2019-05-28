#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "arguments.h"
#include "raw.h"
#include "hexdump.h"
#include "checksums.h"

/*====================================TODO===================================*/
/* Put your required struct definitions */

uint8_t mymac[ETH_ALEN];
uint8_t multimac[ETH_ALEN]={0xff};

typedef struct Frametype {
	uint16_t frametype;
	int frames, bytes;
} Frametype;

/* Put your ancillary functions here*/

/*===========================================================================*/

void assignment2(int fd, int frames)
{
	uint16_t ethtype;
	unsigned int timeout = 10000, i, forme=0, multicast=0;
	uint8_t recbuffer[1514];
	size_t ret, ftsize=0;
	Frametype fts[255]; /* TODO: Make this variable length if it needs to */

	/* If you want to set up any data/counters before the receive loop,
	 * this is the right location
	 */

	memcpy(&mymac, grnvs_get_hwaddr(fd), ETH_ALEN);

	fputs("my mac: ", stderr);
	hexdump(mymac, ETH_ALEN);

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

		hexdump(recbuffer, ret);

		for(i=0; i<ftsize; i++)
		{
			ethtype=recbuffer[12]<<8|recbuffer[13];
			if(ethtype==fts[i].frametype)
				break;
		}

		if(i==ftsize)
		{
			ftsize++;
			fts[i].frametype=ethtype;
			fts[i].frames=1;
			fts[i].bytes=ret-4; /* subtract 4 octets for checksum */
		}
		else
		{
			fts[i].frames++;
			fts[i].bytes+=ret-4; /* again, don't count checksum */
		}

		if(!memcmp(multimac, recbuffer+8, ETH_ALEN))
			multicast++;
		else if(!memcmp(mymac, recbuffer+8, ETH_ALEN))
			forme++;
	}

	/* Print your summary here */
	for(i=0; i<ftsize; i++)
		printf("%04x: %d frames, %d bytes\n", fts[i].frametype, fts[i].frames, fts[i].bytes);
	printf("%d of them were for me\n", forme);
	printf("%d of them were multicast\n", multicast);
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
