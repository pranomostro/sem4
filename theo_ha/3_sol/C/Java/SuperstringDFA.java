import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class SuperstringDFA {

	/**
    * Find the longest suffix of s that is a prefix of w
    * Returns the length of this suffix
    */
	private static int findPrefix(String w, String s) {
		final int n = s.length();
		for (int i = n; i > 0; i--) {
			String suffix = s.substring(n - i, n);
			if (w.startsWith(suffix))
				return i;
		}
		return 0;
	}
	
	/**
    * Construct the DFA that recognizes any words that contain w. (Over the alphabet implicitly defined by w)
    */
	public static DFA superstringDFA(String w) {
		Set<Character> alphabet = new HashSet<>();
		for (char c : w.toCharArray()) { alphabet.add(c); }
		
		State[] states = new State[w.length()+1];
		Set<Transition> transitions = new HashSet<>();
		for (int i = 0; i < states.length; i++)
			states[i] = new State("_" + w.substring(0,i));
		
		for (int i = 0; i < w.length(); i++) {
			for (char c : alphabet) {
				if (w.charAt(i) == c) {
					transitions.add(new Transition(states[i],states[i+1],c));
				} else {
					int prefix = findPrefix(w, w.substring(0,i) + c);
					transitions.add(new Transition(states[i],states[prefix],c));
				}
			}
		}
		
		State finalState = states[w.length()];
		for (char c : alphabet) {
			transitions.add(new Transition(finalState,finalState,c));
		}
		
                Set<State> stateSet = new HashSet<>();
		for (State s : states) { stateSet.add(s); }
		
		return new DFA(stateSet, transitions, alphabet, states[0], Collections.singleton(finalState));
	}
	
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		String s = sc.nextLine();
		DFA d = superstringDFA(s);
		System.out.println(d);
		sc.close();
	}
	
}
