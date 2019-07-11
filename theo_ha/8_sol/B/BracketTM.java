import java.util.Set;

public class BracketTM {
	
	private static final Set<Character> ALPHABET = Set.of('(', ')', '[', ']');

	private static final TuringMachine.Direction R = TuringMachine.Direction.R;
	private static final TuringMachine.Direction L = TuringMachine.Direction.L;
	private static final TuringMachine.Direction N = TuringMachine.Direction.N;

	public static TuringMachine getMachine() {
		TuringMachine.State q0 = new TuringMachine.State("q0");
		TuringMachine tm = new TuringMachine(ALPHABET, q0);

		TuringMachine.State qCheckStackEmpty = new TuringMachine.State("empty?");
		TuringMachine.State qAccept = new TuringMachine.State("accept");
		tm.addFinalState(qAccept);

		TuringMachine.State qPushParGoRight = new TuringMachine.State("push(→1");
		TuringMachine.State qPushParGoRight2 = new TuringMachine.State("push(→2");
		TuringMachine.State qPushBrackGoRight = new TuringMachine.State("push[→1");
		TuringMachine.State qPushBrackGoRight2 = new TuringMachine.State("push[→2");
		TuringMachine.State qGoLeft = new TuringMachine.State("←1");
		TuringMachine.State qGoLeft2 = new TuringMachine.State("←2");

		TuringMachine.State qPopParGoRight = new TuringMachine.State("pop(→1");
		TuringMachine.State qPopParGoRight2 = new TuringMachine.State("pop(→2");
		TuringMachine.State qPopBrackGoRight = new TuringMachine.State("pop[→1");
		TuringMachine.State qPopBrackGoRight2 = new TuringMachine.State("pop[→2");

		TuringMachine.State qPopParCheck = new TuringMachine.State("pop(?");
		TuringMachine.State qPopBrackCheck = new TuringMachine.State("pop[?");

		Set<TuringMachine.State> goRightStates = Set.of(qPushParGoRight, qPushParGoRight2, qPushBrackGoRight,
				qPushBrackGoRight2, qPopBrackGoRight, qPopBrackGoRight2, qPopParGoRight, qPopParGoRight2);

		// If the word has been fully consume: ensure that the stack is empty.
		tm.addTransition(q0, ' ', qCheckStackEmpty, ' ', R);
		tm.addTransition(qCheckStackEmpty, ' ', qAccept, ' ', N);
		
		/*
		 * If the word has not been fully consumed, delete the first character. If it is an opening
		 * paranthesis/bracket, push it to the stack; otherwise, try to pop the corresponding symbol from the
		 * stack.
		 */
		tm.addTransition(q0, '(', qPushParGoRight, ' ', R);
		tm.addTransition(q0, '[', qPushBrackGoRight, ' ', R);
		tm.addTransition(q0, ')', qPopParGoRight, ' ', R);
		tm.addTransition(q0, ']', qPopBrackGoRight, ' ', R);

		// Go to the right until finding a blank.
		for (char c : ALPHABET)
			for (var q : goRightStates)
				tm.addTransition(q, c, q, c, R);

		// End of the input word has been found; now go to the end of the stack.
		tm.addTransition(qPushParGoRight, ' ', qPushParGoRight2, ' ', R);
		tm.addTransition(qPushBrackGoRight, ' ', qPushBrackGoRight2, ' ', R);
		tm.addTransition(qPopParGoRight, ' ', qPopParGoRight2, ' ', R);
		tm.addTransition(qPopBrackGoRight, ' ', qPopBrackGoRight2, ' ', R);
		
		// End of the stack has been found. Write the symbol to be pushed, then go left.
		tm.addTransition(qPushParGoRight2, ' ', qGoLeft, '(', L);
		tm.addTransition(qPushBrackGoRight2, ' ', qGoLeft, '[', L);
		
		// End of the stack has been found. Go one to the left to the uppermost stack symbol.
		tm.addTransition(qPopParGoRight2, ' ', qPopParCheck, ' ', L);
		tm.addTransition(qPopBrackGoRight2, ' ', qPopBrackCheck, ' ', L);

		// Ensure that the uppermost stack symbol matches the one we want to pop.
		tm.addTransition(qPopParCheck, '(', qGoLeft, ' ', L);
		tm.addTransition(qPopBrackCheck, '[', qGoLeft, ' ', L);

		// Go all the way to the left, i.e. to the beginning of the input word.
		for (char c : ALPHABET) {
			tm.addTransition(qGoLeft, c, qGoLeft, c, L);
			tm.addTransition(qGoLeft2, c, qGoLeft2, c, L);
		}
		tm.addTransition(qGoLeft, ' ', qGoLeft2, ' ', L);
		tm.addTransition(qGoLeft2, ' ', q0, ' ', R);

		return tm;
	}
	
	public static void main(String[] args) {
		System.out.println(getMachine());
	}
	
}
