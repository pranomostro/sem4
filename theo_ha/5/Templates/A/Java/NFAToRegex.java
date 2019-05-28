import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

public class NFAToRegex {
	
	public static boolean fullMode = false; // flag that decides if intermediate tables are printed

	private final NFA nfa;
	private Regex result;

	public NFAToRegex(NFA nfa) {
		this.nfa = nfa;
		runComputation();
	}
	
	/**
	 * Returns the regular expression stored in the table for the given pair of
	 * states. If there is none, returns Regex.empty().
	 */
	private Regex get(Map<Pair<State, State>, Regex> m, State q1, State q2) {
		Regex r = m.get(new Pair<>(q1, q2));
		if (r == null)
			return Regex.empty();
		else
			return r;
	}
	
	/**
	 * Stores a regular expression in the table for the given pair of states.
	 */
	private void put(Map<Pair<State, State>, Regex> m, State q1, State q2, Regex r) {
		m.put(new Pair<>(q1, q2), r);
	}
	
	public NFA getNFA() {
		return nfa;
	}

	/**
	 * Returns the set of all characters leading from the first state to the second
	 * state in the automaton.
	 */
	private Set<Character> getLabelsFromTo(State q1, State q2) {
		Set<Character> s = new HashSet<>();
		for (Transition t : nfa.getTransitions()) {
			if (t.getStart().equals(q1) && t.getEnd().equals(q2))
				s.add(t.getLabel());
		}
		return s;
	}

	/**
	 * Initialises the table for k = 0, i.e. computes all the α_{ij}^0 and stores
	 * them in the table.
	 * 
	 * TODO Implement this (corresponds to the k = 0 case in Theorem 3.19)
	 * Return the content of the table α_{ij}^0
	 */
	private Map<Pair<State, State>, Regex> initTable() {
		return null;
	}

	/**
	 * Performs the step k → k + 1 as explained on the lecture slides, i.e. turns a
	 * table of values for α_{ij}^k into a table with the values of α_{ij}^{k+1}.
	 * 
	 * TODO Implement this (corresponds to the induction step k → k + 1 in Theorem 3.19)
	 * Given the table α_{ij}^k and the state qk, return the table α_{ij}^{k+1}.
	 */
	private Map<Pair<State, State>, Regex> step(Map<Pair<State, State>, Regex> table, State qk) {
		return null;
	}

	/**
	 * Computes the regular expression equivalent to the NFA from the filled table.
	 * 
	 * TODO Implement this (corresponds to the last step of Theorem 3.19)
	 * Given the filled table α_{ij}^n, read off the regular expression that is
	 * equivalent to the entire NFA and return it.
	 */
	private Regex computeResult(Map<Pair<State, State>, Regex> table) {
		return null;
	}
	
	/**
	 * Runs the construction from Theorem 3.19.
	 * You do /not/ have to modify this code at all. This just calls the three
	 * different steps outlined above and takes care of the printing.
	 */
	private void runComputation() {
		if (fullMode)
			System.out.println("BEGIN NFA TO REGEX TRACE");
		var table = initTable(); // Initialisation: k = 0
		int i = 0;
		printTable(i, null, table);
		for (State q : nfa.getStates()) { // n steps k → k + 1, one for each state
			table = step(table, q);
			System.out.println();
			printTable(++i, q, table);
		}
		result = computeResult(table); // read off result
		printResult();
		if (fullMode)
			System.out.println("END NFA TO REGEX TRACE");
	}

	
	public Regex getResult() {
		return result;
	}
	
	// BEGIN HISTORY PRINTING CODE
	// Ignore this code.
	private void printTable(int i, State q, Map<Pair<State, State>, Regex> m) {
		if (!fullMode) return;
		System.out.println("Step " + i);
		if (q != null)
			System.out.println("Processing state " + q);
		for (State q1 : nfa.getStates())
			for (State q2 : nfa.getStates())
				System.out.printf("(%s, %s): %s\n", q1, q2, get(m, q1, q2));
	}
	
	private void printResult() {
		if (fullMode)
		  System.out.println("\nFinal result: " + result);
	}
	// END HISTORY PRINTING CODE
	
	

	/**
	 * Converts the given NFA into an equivalent regular expression
	 */
	public static Regex nfaToRegex(NFA nfa) {
		return (new NFAToRegex(nfa)).getResult();
	}
	
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		String mode = sc.nextLine();
		if (mode.equals("Full")) {
			fullMode = true;
		} else if (mode.equals("Result only")) {
			fullMode = false;
		} else {
			sc.close();
			System.err.println("Invalid mode.");
			System.exit(-1);
			return;
		}
		
		while (sc.hasNext()) {
			NFA nfa = (NFA) Parser.parse(sc);
			NFAToRegex toRegex = new NFAToRegex(nfa);
			if (!fullMode)
				System.out.println(toRegex.getResult());
		}
		sc.close();
	}

}
