

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Stream;

public class RegexRemainder implements Regex.Visitor<Regex> {

	public final char c;
	
	public RegexRemainder(char c) {
		this.c = c;
	}
	
	// TODO: add unimplemented methods here
	
	
	// END TODO
	
	public static final Regex remainder(Regex r, char c) {
		return r.accept(new RegexRemainder(c));
	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		while (sc.hasNextLine()) {
			String line = sc.nextLine();
			if (line.equals("END")) break;
			String[] s = line.split(";");
			if (s.length != 2 || s[0].trim().length() != 1) {
				System.err.println("Illegal line in input: " + line);
				System.exit(-1);
				break;
			}
			char c = s[0].trim().charAt(0);
			Regex r = Regex.parse(s[1]);
			System.out.println(RegexRemainder.remainder(r, c));
		}
		sc.close();
	}
	
}
