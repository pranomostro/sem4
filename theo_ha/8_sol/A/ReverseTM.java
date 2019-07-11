import java.util.Set;

public class ReverseTM {
	
	private static final Set<Character> ALPHABET = Set.of('0', '1');

	private static final TuringMachine.Direction R = TuringMachine.Direction.R;
	private static final TuringMachine.Direction L = TuringMachine.Direction.L;
	private static final TuringMachine.Direction N = TuringMachine.Direction.N;

	public static TuringMachine getMachine() {
		TuringMachine.State q0 = new TuringMachine.State("q0");
		TuringMachine tm = new TuringMachine(ALPHABET, q0);

		TuringMachine.State copy = new TuringMachine.State("cp");
		TuringMachine.State copy0Right = new TuringMachine.State("cp0→");
		TuringMachine.State copy1Right = new TuringMachine.State("cp1→");
		TuringMachine.State copyLeft1 = new TuringMachine.State("cp←");
		TuringMachine.State copyLeft2 = new TuringMachine.State("cp←←");
		TuringMachine.State cleanRight = new TuringMachine.State("clean→");
		TuringMachine.State cleanLeft = new TuringMachine.State("clean←");
		TuringMachine.State halt = new TuringMachine.State("HALT");
		
		tm.addFinalState(halt);
		
		/*
		 * Idea: We mark all the letters we have already copied. The marked version of 0 is A, 
		 * the marked version of 1 is B.
		 */
		
		// Initial positioning step: Go all the way to the right
		tm.addTransition(q0, '0', q0, '0', R);
		tm.addTransition(q0, '1', q0, '1', R);
		tm.addTransition(q0, ' ', copy, ' ', L);
		
		// Mark the current digit.
		tm.addTransition(copy, '0', copy0Right, 'A', R);
		tm.addTransition(copy, '1', copy1Right, 'B', R);
		tm.addTransition(copy, ' ', cleanRight, ' ', R);
		
		// Go all the way to the right and place a 0 or 1 there
		for (char c : Set.of('0', '1', 'A', 'B')) {
			for (var q : Set.of(copy0Right, copy1Right))
				tm.addTransition(q, c, q, c, R);
		}
		tm.addTransition(copy0Right, ' ', copyLeft1, '0', L);
		tm.addTransition(copy1Right, ' ', copyLeft1, '1', L);
		
		// Go to the left to the rightmost marked digit
		tm.addTransition(copyLeft1, '0', copyLeft1, '0', L);
		tm.addTransition(copyLeft1, '1', copyLeft1, '1', L);
		tm.addTransition(copyLeft1, 'A', copyLeft2, 'A', L);
		tm.addTransition(copyLeft1, 'B', copyLeft2, 'B', L);
		
		// Go to the left to the rightmost unmarked digit
		tm.addTransition(copyLeft2, 'A', copyLeft2, 'A', L);
		tm.addTransition(copyLeft2, 'B', copyLeft2, 'B', L);
		tm.addTransition(copyLeft2, '0', copy, '0', N);
		tm.addTransition(copyLeft2, '1', copy, '1', N);
		tm.addTransition(copyLeft2, ' ', copy, ' ', N);
		
		/*
		 *  After the entire word has been copied: go all the way to the right and
		 *  replace the marked digits by their unmarked equivalents.
		 */
		tm.addTransition(cleanRight, '0', cleanRight, '0', R);
		tm.addTransition(cleanRight, '1', cleanRight, '1', R);
		tm.addTransition(cleanRight, 'A', cleanRight, '0', R);
		tm.addTransition(cleanRight, 'B', cleanRight, '1', R);
		tm.addTransition(cleanRight, ' ', cleanLeft, ' ', L);
		
		// Last step: Go back to the beginning of the tape.
		tm.addTransition(cleanLeft, '0', cleanLeft, '0', L);
		tm.addTransition(cleanLeft, '1', cleanLeft, '1', L);
		tm.addTransition(cleanLeft, ' ', halt, ' ', R);
		
		return tm;
	}
	
	public static void main(String[] args) {
		System.out.println(getMachine());
	}
	
}
