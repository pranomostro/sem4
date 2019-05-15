import java.util.Scanner;
import java.util.TreeSet;

public class Test {

	private static void testLanguage(Regex r) {
		TreeSet<String> set = r.enumerateWords();
		if (set == null) {
			System.out.println("infinite");
		} else {
			System.out.print("{");
			boolean first = true;
			for (String w : set) {
				if (!first)
					System.out.print(", ");
				first = false;
				System.out.print((w.isEmpty() ? "ε" : w));
			}
			System.out.println("}");
		}
	}
	
	private static void testTrivial(Regex r) {
		System.out.println(r.isTrivial() ? "yes" : "no");
	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		String mode = sc.nextLine();
		while (sc.hasNextLine()) {
			String s = sc.nextLine();
			if (s.equals("END")) break;
			Regex r = Regex.parse(s);
			if (mode.equals("Language"))
				testLanguage(r);
			else
				testTrivial(r);
		}
		sc.close();
	}

}
