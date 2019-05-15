import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class SuperstringDFA {

	/**
    * Find the longest suffix of s that is a prefix of w
    * Returns the length of this suffix
    * 
    * You do not /have/ to implement this, but using an auxiliary function like this is recommended.
    */
	private static int findPrefix(String w, String s) {
		// TODO
		return 0;
	}
	
	/**
    * Construct the DFA that recognizes any words that contain w. (Over the alphabet implicitly defined by w)
    */
	public static DFA superstringDFA(String w) {
		Set<Character> alphabet = new HashSet<>();
		for (char c : w.toCharArray()) { alphabet.add(c); }
		
		// TODO
		return null;
	}
	
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		String s = sc.nextLine();
		DFA d = superstringDFA(s);
		System.out.println(d);
		sc.close();
	}
	
}
