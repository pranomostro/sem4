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

/* Put your ancillary functions here*/

/*===========================================================================*/

void assignment2(int fd, int frames)
{
	unsigned int timeout = 10000;
	uint8_t recbuffer[1514];
	size_t ret;

/*====================================TODO===================================*/
	/* If you want to set up any data/counters before the receive loop,
	 * this is the right location
	 */

	memcpy(&mymac, grnvs_get_hwaddr(fd), ETH_ALEN);

/*===========================================================================*/

	/* This is the ready marker! do not remove! */
	fprintf(stdout, "I am ready!\n");

/*====================================TODO===================================*/
	/* Update the loop condition */
	while(1) {
/*===========================================================================*/
		ret = grnvs_read(fd, recbuffer, sizeof(recbuffer), &timeout);
		if (ret == 0) {
			fprintf(stderr, "Timed out, this means there was nothing to receive. Do you have a sender set up?\n");
			break;
		}
/*====================================TODO===================================*/
	/* This is the receive loop, 'recbuffer' will contain the received
	 * frame. 'ret' tells you the length of what you received.
	 * Anything that should be done with every frame that's received
	 * should be done here.
	 */

	printf("%d\n", ret);

/*===========================================================================*/
	}

/*====================================TODO===================================*/
	/* Print your summary here */

/*===========================================================================*/
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
