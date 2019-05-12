/* Do NOT!!! put a package statement here, that would break the build system */

public class Assignment1 {

	public static void run(long max) {
/*====================================TODO===================================*/

/*===========================================================================*/
	}



	public static void main(String[] argv) {
		Arguments args = new Arguments(argv);
		try{
			run(args.max);
		}
		catch(Exception e) {
			e.printStackTrace();
			System.out.println(e.getMessage());
			System.exit(1);
		}
	}
}
