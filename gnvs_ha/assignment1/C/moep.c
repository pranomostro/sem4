#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

int main(int argc, char** argv)
{
	long i, c;

	if(argc!=2)
	{
		fprintf(stderr, "moep num\n");
		exit(1);
	}

	i=strtol(argv[1], NULL, 10);

	if(errno!=0)
	{
		fprintf(stderr, "expected number as first argument, exiting\n");
		exit(2);
	}

	for(c=1;c<=i;c++)
	{
		if(c%3==0&&c%4==0)
			puts("MoepWow!");
		else if(c%3==0)
			puts("Moep");
		else if(c%4==0)
			puts("Wow!");
		else
			printf("%ld\n", c);
	}

	return 0;
}
