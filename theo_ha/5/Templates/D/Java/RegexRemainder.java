

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
	
	// TODO: Add your implementation from exercise C here
	
	public static final Regex remainder(Regex r, char c) {
		return r.accept(new RegexRemainder(c));
	}
	
	public static final Regex remainder(Regex r, String w) {
		return null; // TODO
	}
	
	public static final boolean matches(Regex r, String w) {
		return false; // TODO
	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		while (sc.hasNextLine()) {
			String line = sc.nextLine();
			if (line.equals("END")) break;
			String[] s = line.split(";");
			if (s.length < 2) {
				System.err.println("Illegal line in input: " + line);
				System.exit(-1);
				break;
			}
			Regex r = Regex.parse(s[0]);
			String[] match = new String[s.length - 1];
            for (int i = 1; i < s.length; i++)
			  match[i - 1] = s[i] + ": " + ((RegexRemainder.matches(r, s[i])) ? "y" : "n");
			System.out.println(String.join("; ", match));
		}
		sc.close();
	}
	
}
