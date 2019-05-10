class Arguments {
	public long max;

	private void printHelp() {
		System.out.println("This is the first exercise of grnvs 2019");
		System.out.println("Usage:");
		System.out.println("max:            The number to loop to");
		System.out.println("-?/--help	    Print this help message");
	}

	Arguments(String[] argv) {
		if(argv.length == 0) {
			printHelp();
			System.exit(0);
		}
		//For_each would be nice, but we may have to skip/access next
		int i, j = 0;
		String[] fargs = new String[1];
		for(i = 0; i < argv.length; ++i) {
			String arg = argv[i];
			switch(arg) {
				case "-?":
				case "--help":
					printHelp();
					System.exit(0);
					break;
				default:
					if(j == fargs.length) {
						System.out.println("Encountered an unexpected number of positional arguments");
						System.exit(1);
					}
					fargs[j++] = arg;
					break;
			}
		}
		if(fargs[0] == null) {
			System.out.println("Did not find positional argument: max");
			System.exit(1);
		}
		// read int fargs[0]
		max = Long.parseLong(fargs[0]);;
	}
}
