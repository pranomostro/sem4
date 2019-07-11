import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

public class TuringMachine {

	public static class State {

		private static int maxStateId = -1;
		public final int id;
		public final String name;

		public State() {
			this.id = ++maxStateId;
			this.name = null;
		}

		public State(String name) {
			this.id = ++maxStateId;
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + id;
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			State other = (State) obj;
			if (id != other.id)
				return false;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			return true;
		}

		public String toString() {
			if (name != null && !name.isEmpty())
				return name + "_" + id;
			else
				return Integer.toString(id);
		}

		public String toString(Set<String> uniqueNames) {
			if (name != null && uniqueNames.contains(name))
				return name;
			else if (name != null && !name.isEmpty())
				return name + "_" + id;
			else
				return Integer.toString(id);
		}

	}

	public static final char EMPTY_LETTER = ' ';
	public static final char ANY_LETTER = '*';

	private final Set<State> states;
	private final Set<Character> alphabet;
	private final Map<State, Map<char[], Transition>> successorFunction;
	private State initialState;
	private final Set<State> finalStates;
	public final int numberOfTapes;

	public TuringMachine(Set<Character> alphabet, State initialState, int numberOfTapes) {
		if (alphabet.contains(EMPTY_LETTER)) {
			throw new IllegalArgumentException("Letter " + EMPTY_LETTER + " is reserved");
		}

		this.states = new HashSet<>();
		this.alphabet = Collections.unmodifiableSet(new HashSet<>(alphabet));
		this.successorFunction = new HashMap<>();
		this.initialState = Objects.requireNonNull(initialState);
		this.finalStates = new HashSet<>();
		this.numberOfTapes = numberOfTapes;
		this.states.add(initialState);

		NO_CHANGE = replicateLetters('*');
		ANYTHING = NO_CHANGE;
		DONT_MOVE = replicateDirections(Direction.N);
	}

	public void addFinalState(State state) {
		states.add(state);
		finalStates.add(state);
	}

	public void addFinalStates(Collection<State> states) {
		this.states.addAll(states);
		finalStates.addAll(states);
	}

	public void removeStates(Collection<State> states) {
		if (states.contains(initialState)) {
			throw new IllegalArgumentException("Removing initial state");
		}
		successorFunction.keySet().removeAll(states);
		successorFunction
				.forEach((state, successors) -> successors.values().removeIf(t -> states.contains(t.successor)));
		finalStates.removeAll(states);
		this.states.removeAll(states);
	}

	public void setInitialState(State state) {
		states.add(state);
		initialState = state;
	}

	public void addTransition(State sourceState, char letters[], State successor, char replacements[],
			Direction[] directions) {
		checkValid(letters);
		checkValid(replacements);
		if (directions.length != numberOfTapes)
			throw new IllegalArgumentException("Wrong number of directions: " + Arrays.toString(directions));

		char letters2[] = Arrays.copyOf(letters, letters.length);

		states.addAll(Arrays.asList(sourceState, successor));
		successorFunction.computeIfAbsent(sourceState, k -> new HashMap<>()).put(letters2,
				new Transition(successor, replacements, directions));
	}

	public String stateToString(State q) {
		return q.toString(getUniqueNames());
	}

	public Set<State> getStates() {
		return Collections.unmodifiableSet(states);
	}

	public Set<Character> getAlphabet() {
		return alphabet;
	}

	public State getInitialState() {
		return initialState;
	}

	public Set<State> getFinalStates() {
		return Collections.unmodifiableSet(finalStates);
	}

	public boolean isFinal(State state) {
		return finalStates.contains(state);
	}

	public void setFinal(State state, boolean isFinal) {
		if (isFinal)
			addFinalState(state);
		else
			finalStates.remove(state);
	}

	public void setFinalStates(Set<State> finalStates) {
		states.addAll(finalStates);
		this.finalStates.clear();
		this.finalStates.addAll(finalStates);
	}

	/**
	 * Adds all states, final states, and transitions from the given other TM to
	 * this TM. The alphabets and number of tapes of the two machines have to agree
	 * and their states must be disjoint.
	 * 
	 * After this operation, the set of states/final states/transitions of this TM
	 * will be the union of the states/final states/transitions of the two TMs. The
	 * initial states it still the initial state of this TM.
	 * 
	 * The other TM remains unchanged.
	 */
	public void addMachine(TuringMachine other) {
		if (!alphabet.equals(other.alphabet))
			throw new IllegalArgumentException("Turing machines have different alphabets.");
		if (numberOfTapes != other.numberOfTapes)
			throw new IllegalArgumentException(String.format(
					"Turing machines have different number of tapes: %d vs. %d", numberOfTapes, other.numberOfTapes));

		for (State q : states)
			if (other.states.contains(q))
				throw new IllegalArgumentException("Sets of states are not disjoint.");

		for (var entry1 : other.successorFunction.entrySet()) {
			for (var entry2 : entry1.getValue().entrySet()) {
				Transition t = entry2.getValue();
				addTransition(entry1.getKey(), entry2.getKey(), t.successor, t.letters, t.directions);
			}
		}

		states.addAll(other.states);
		addFinalStates(other.finalStates);
	}

	public static TuringMachine idle(Set<Character> alphabet, int numberOfTapes) {
		State q = new State();
		TuringMachine m = new TuringMachine(alphabet, q, numberOfTapes);
		m.addFinalState(q);
		return m;
	}

	/**
	 * Turns this TM into the composition of this TM and the given other TM. The
	 * other TM remains unchanged.
	 */
	public void compose(TuringMachine other) {
		Set<State> fin = new HashSet<>(finalStates);
		addMachine(other);

		// Add transitions from all final states of TM 1 to the initial state of TM 2
		for (State q : fin)
			addTransition(q, ANYTHING, other.initialState, NO_CHANGE, DONT_MOVE);

		// Make final states of TM 1 non-final.
		setFinalStates(other.finalStates);
	}

	
	
	// YOU CAN IGNORE THE REST OF THE FILE
	
	private void checkValid(char letter) {
		if (letter != EMPTY_LETTER && letter != ANY_LETTER && !alphabet.contains(letter)) {
			throw new IllegalArgumentException("Invalid letter " + letter);
		}
	}

	private void checkValid(char[] letters) {
		if (letters.length != numberOfTapes)
			throw new IllegalArgumentException("Wrong number of characters: " + Arrays.toString(letters));
		for (char letter : letters)
			checkValid(letter);
	}
	
	private String arrayToString(char[] a) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < a.length; i++)
			sb.append(a[i]);
		return sb.toString();
	}

	private String prettyArrayToString(char[] a) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < a.length; i++)
			if (a[i] == EMPTY_LETTER)
				sb.append('□');
			else
				sb.append(a[i]);
		return sb.toString();
	}

	private String arrayToString(Object[] a) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < a.length; i++)
			sb.append(a[i]);
		return sb.toString();
	}

	private Set<String> getUniqueNames() {
		Map<String, Integer> m = new HashMap<>();
		for (State q : states) {
			if (q.name != null) {
				Integer n = m.get(q.name);
				m.put(q.name, (n == null) ? 1 : n + 1);
			}
		}

		Set<String> s = new HashSet<>();
		for (var e : m.entrySet()) {
			if (e.getValue() == 1)
				s.add(e.getKey());
		}
		return s;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(successorFunction.size() * 20);
		Set<String> unique = getUniqueNames();

		alphabet.stream().sorted().forEach(c -> sb.append(c));
		sb.append("\n");
		sb.append(initialState.toString(unique)).append("\n");
		sb.append(String.join(";", finalStates.stream().map(q -> q.toString(unique)).sorted().toArray(String[]::new)));
		sb.append("\n");
		sb.append(numberOfTapes).append("\n");

		List<String> transitionStrings = new LinkedList<>();
		for (var e : successorFunction.entrySet()) {
			String q = e.getKey().toString(unique);
			for (var e2 : e.getValue().entrySet()) {
				char[] letters = e2.getKey();
				Transition t = e2.getValue();
				String s = String.join(";", q, arrayToString(letters), t.successor.toString(unique),
						arrayToString(t.letters), arrayToString(t.directions));
				transitionStrings.add(s);
			}
		}
		transitionStrings.stream().sorted().forEach(s -> sb.append(s).append("\n"));
		sb.append("END");

		return sb.toString();
	}

	private static String charToDot(char c) {
		if (c == EMPTY_LETTER)
			return "□";
		else
			return Character.toString(c);
	}

	private static String printLettersAndDirections(char[] letters, char[] replacements, Direction[] directions) {
		String[] a = new String[letters.length];
		for (int i = 0; i < letters.length; i++) {
			a[i] = charToDot(letters[i]);
		}

		String[] b = new String[2 * replacements.length];
		for (int i = 0; i < replacements.length; i++) {
			b[2 * i] = charToDot(replacements[i]);
			b[2 * i + 1] = directions[i].toString();
		}
		return String.join(",", a) + " / " + String.join(",", b);
	}

	public String toDot() {
		StringBuilder dotBuilder = new StringBuilder(successorFunction.size() * 20);
		dotBuilder.append("digraph turing {\n").append("  node [fontname = \"Roboto\"];\n") // Nice node font
				.append("  edge [fontname = \"Courier\"];"); // Monospaced edge font

		states.forEach(state -> dotBuilder.append("  ").append("node [shape=")
				.append(finalStates.contains(state) ? "doublecircle" : "circle").append("] \"").append(state)
				.append("\";\n"));

		Map<State, Map<State, Set<String>>> fromToLabels = new HashMap<>();

		successorFunction.forEach((state, successors) -> {
			Map<State, Set<String>> successorLabels = new HashMap<>();
			successors.forEach(
					(letter, transition) -> successorLabels.computeIfAbsent(transition.successor, k -> new HashSet<>())
							.add(printLettersAndDirections(letter, transition.letters, transition.directions)));
			if (!successorLabels.isEmpty()) {
				fromToLabels.put(state, successorLabels);
			}
		});

		fromToLabels.forEach((state, successors) -> successors.forEach((successor, labels) -> {
			dotBuilder.append("  \"").append(state).append("\" -> \"").append(successor).append("\" [label=\"")
					.append(String.join("\\n", labels)).append("\"];\n");
		}));

		dotBuilder.append("}");
		return dotBuilder.toString();
	}

	public static boolean match(char[] pattern, char[] letters) {
		for (int i = 0; i < pattern.length; i++)
			if (pattern[i] != '*' && pattern[i] != letters[i])
				return false;
		return true;
	}

	public Transition getTransition(State state, char[] letters) {
		if (!states.contains(state)) {
			throw new IllegalArgumentException("Unknown state " + state);
		}
		Map<char[], Transition> m = successorFunction.get(state);
		if (m == null)
			return null;

		Transition t = null;
		for (var e : m.entrySet()) {
			if (match(e.getKey(), letters)) {
				if (t != null) {
					throw new IllegalStateException(
							String.format("Non-deterministic transition from state %s with letters '%s'.",
									stateToString(state), prettyArrayToString(letters)));
				} else {
					t = e.getValue();
				}
			}
		}
		return t;
	}

	private static char toChar(String str) {
		if (str.length() != 1) {
			throw new IllegalArgumentException();
		}
		return str.charAt(0);
	}
	
	private static State makeState(Map<String, State> stateMap, String name) {
		State q = stateMap.get(name);
		if (q == null) {
			q = new State(name);
			stateMap.put(name, q);
		}
		return q;
	}

	public static TuringMachine parse(BufferedReader reader) throws IOException {
		String alphabetLine = reader.readLine();
		char[] alphabetChars = alphabetLine.toCharArray();
		Set<Character> alphabet = new HashSet<>();
		for (char chr : alphabetChars) {
			alphabet.add(chr);
		}

		Map<String, State> stateMap = new HashMap<>();
		String initial = reader.readLine();
		stateMap.put(initial, makeState(stateMap, initial));

		String[] finalStates = reader.readLine().split(";");

		int numberOfTapes = Integer.parseInt(reader.readLine());

		TuringMachine tm = new TuringMachine(alphabet, stateMap.get(initial), numberOfTapes);
		for (String name : finalStates) {
			State q = makeState(stateMap, name);
			stateMap.put(name, q);
			tm.addFinalState(q);
		}

		String line;
		while (!Objects.equals(line = reader.readLine(), "END")) {
			String[] split = line.split(";");
			if (split.length != 5) {
				throw new IllegalArgumentException("Invalid transition " + line);
			}

			String source = split[0];
			char[] sourceLetters = split[1].toCharArray();
			String successor = split[2];
			char replacements[] = split[3].toCharArray();
			Direction[] directions = new Direction[split[4].length()];
			for (int i = 0; i < split[4].length(); i++)
				directions[i] = Direction.valueOf(Character.toString(split[4].charAt(i)));

			State q1 = stateMap.get(source);
			if (q1 == null) {
				q1 = makeState(stateMap, source);
				stateMap.put(source, q1);
			}

			State q2 = stateMap.get(successor);
			if (q2 == null) {
				q2 = makeState(stateMap, successor);
				stateMap.put(successor, q2);
			}

			tm.addTransition(q1, sourceLetters, q2, replacements, directions);
		}

		return tm;
	}

	public static enum Direction {
		L, R, N
	}

	public char[] makeLetters(Function<Integer, Character> f) {
		char[] a = new char[numberOfTapes];
		for (int i = 0; i < numberOfTapes; i++)
			a[i] = f.apply(i);
		return a;
	}

	public TuringMachine.Direction[] makeDirections(Function<Integer, TuringMachine.Direction> f) {
		TuringMachine.Direction[] a = new TuringMachine.Direction[numberOfTapes];
		for (int i = 0; i < numberOfTapes; i++)
			a[i] = f.apply(i);
		return a;
	}

	public char[] replicateLetters(char c) {
		char[] a = new char[numberOfTapes];
		Arrays.fill(a, c);
		return a;
	}

	public TuringMachine.Direction[] replicateDirections(TuringMachine.Direction d) {
		TuringMachine.Direction[] a = new TuringMachine.Direction[numberOfTapes];
		Arrays.fill(a, d);
		return a;
	}

	public final char[] ANYTHING;
	public final char[] NO_CHANGE;
	public final Direction[] DONT_MOVE;

	public final class Transition {
		public final State successor;
		public final char[] letters;
		public final Direction[] directions;

		public Transition(State successor, char[] letters, Direction[] directions) {
			this.successor = successor;
			this.letters = Arrays.copyOf(letters, directions.length);
			this.directions = Arrays.copyOf(directions, directions.length);
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getEnclosingInstance().hashCode();
			result = prime * result + Arrays.hashCode(directions);
			result = prime * result + Arrays.hashCode(letters);
			result = prime * result + ((successor == null) ? 0 : successor.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Transition other = (Transition) obj;
			if (!getEnclosingInstance().equals(other.getEnclosingInstance()))
				return false;
			if (!Arrays.equals(directions, other.directions))
				return false;
			if (!Arrays.equals(letters, other.letters))
				return false;
			if (successor == null) {
				if (other.successor != null)
					return false;
			} else if (!successor.equals(other.successor))
				return false;
			return true;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append("(").append(successor);
			for (int i = 0; i < numberOfTapes; i++)
				sb.append(",").append(letters[i]).append(",").append(directions[i]);
			sb.append(")");
			return sb.toString();
		}

		private TuringMachine getEnclosingInstance() {
			return TuringMachine.this;
		}
	}

	
	
	public class OverlappingTransitionsException extends Exception {
		Transition2 t1, t2;
		
		public OverlappingTransitionsException(Transition2 t1, Transition2 t2) {
			super();
			this.t1 = t1;
			this.t2 = t2;
		}

		public String getMessage() {
			return String.format("TM is not deterministic: The following two transitions overlap:\n  %s\n  %s", t1, t2);
		}
		
	}

	private abstract class TrieNode {
		public abstract void add(int depth, Transition2 t) throws OverlappingTransitionsException;
	}
	
	private TrieNode makeTrie(int depth, Transition2 t) throws OverlappingTransitionsException {
		TrieNode trie;
		if (depth == t.letters.length) {
			trie = new TrieLeaf(t);
		} else {
			trie = new InternalTrieNode();
			trie.add(depth, t);
		}
		return trie;
	}
	
	private class InternalTrieNode extends TrieNode {
		Map<Character, TrieNode> children = new HashMap<>();
		
		private void add(char c, int depth, Transition2 t) throws OverlappingTransitionsException {
			TrieNode child = children.get(c);
			if (child == null) {
				child = makeTrie(depth + 1, t);
				children.put(c, child);
			} else {
				child.add(depth + 1, t);
			}
		}
		
		public void add(int depth, Transition2 t) throws OverlappingTransitionsException {
			char c = t.letters[depth];
			if (c == ANY_LETTER) {
				add(EMPTY_LETTER, depth, t);
				for (char c2 : alphabet)
					add(c2, depth, t);
			} else {
				add(c, depth, t);
			}
		}

	}
	
	private class TrieLeaf extends TrieNode {
		Transition2 t;
		public TrieLeaf(Transition2 t) {
			this.t = t;
		}
		
		public void add(int depth, Transition2 t) throws OverlappingTransitionsException {
			throw new OverlappingTransitionsException(this.t, t);
		}
	}
	
	public class Transition2 {
		State q1, q2;
		char[] letters, replacements;
		Direction[] directions;

		public Transition2(State q1, State q2, char[] letters, char[] replacements, Direction[] directions) {
			super();
			this.q1 = q1;
			this.q2 = q2;
			this.letters = letters;
			this.replacements = replacements;
			this.directions = directions;
		}
		
		public String toString() {
			return String.format("%s;%s;%s;%s;%s", stateToString(q1), arrayToString(letters), stateToString(q2), 
					arrayToString(replacements), arrayToString(directions));
		}
	}
	
	public void checkDeterminism() throws OverlappingTransitionsException {
		for (var e1 : successorFunction.entrySet()) {
			TrieNode trie = null;
			for (var e2 : e1.getValue().entrySet()) {
				Transition t = e2.getValue();
				Transition2 t2 = new Transition2(e1.getKey(), t.successor, e2.getKey(), t.letters, t.directions);
				if (trie == null)
					trie = makeTrie(0, t2);
				else
					trie.add(0, t2);
			}
		}
	}

}