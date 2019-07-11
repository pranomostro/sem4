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
import java.util.Optional;
import java.util.Set;

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
	private final Map<State, Map<Character, Transition>> successorFunction;
	private State initialState;
	private final Set<State> finalStates;

	public TuringMachine(Set<Character> alphabet, State initialState) {
		if (alphabet.contains(EMPTY_LETTER)) {
			throw new IllegalArgumentException("Letter " + EMPTY_LETTER + " is reserved");
		}

		this.states = new HashSet<>();
		this.alphabet = Collections.unmodifiableSet(new HashSet<>(alphabet));
		this.successorFunction = new HashMap<>();
		this.initialState = Objects.requireNonNull(initialState);
		this.finalStates = new HashSet<>();
		this.states.add(initialState);
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

	public void addTransition(State sourceState, char letter, State successor, char replacement, Direction direction) {
		states.addAll(Arrays.asList(sourceState, successor));
		successorFunction.computeIfAbsent(sourceState, k -> new HashMap<>()).put(letter,
				new Transition(successor, replacement, direction));
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

	// YOU CAN IGNORE THE REST OF THE FILE

	private void checkValid(char letter) {
		if (letter != EMPTY_LETTER && letter != ANY_LETTER && !alphabet.contains(letter)) {
			throw new IllegalArgumentException("Invalid letter " + letter);
		}
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

		List<String> transitionStrings = new LinkedList<>();
		for (var e : successorFunction.entrySet()) {
			String q = e.getKey().toString(unique);
			for (var e2 : e.getValue().entrySet()) {
				char letter = e2.getKey();
				Transition t = e2.getValue();
				String s = String.join(";", q, Character.toString(letter), t.successor.toString(unique),
						Character.toString(t.letter), t.direction.toString());
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

	public String toDot() {
		Set<String> unique = getUniqueNames();
		StringBuilder dotBuilder = new StringBuilder(successorFunction.size() * 20);
		dotBuilder.append("digraph turing {\n").append("  node [fontname = \"Roboto\"];\n") // Nice node font
				.append("  edge [fontname = \"Courier\"];"); // Monospaced edge font

		states.forEach(state -> dotBuilder.append("  ").append("node [shape=")
				.append(finalStates.contains(state) ? "doublecircle" : "circle").append("] \"")
				.append(state.toString(unique)).append("\";\n"));

		Map<State, Map<State, Set<String>>> fromToLabels = new HashMap<>();

		successorFunction.forEach((state, successors) -> {
			Map<State, Set<String>> successorLabels = new HashMap<>();
			successors.forEach((letter, transition) -> successorLabels
					.computeIfAbsent(transition.successor, k -> new HashSet<>()).add(String.format("%s/%s,%s",
							charToDot(letter), charToDot(transition.letter), transition.direction)));
			if (!successorLabels.isEmpty()) {
				fromToLabels.put(state, successorLabels);
			}
		});

		fromToLabels.forEach((state, successors) -> successors.forEach((successor, labels) -> {
			dotBuilder.append("  \"").append(state.toString(unique)).append("\" -> \"")
					.append(successor.toString(unique)).append("\" [label=\"").append(String.join("\\n", labels))
					.append("\"];\n");
		}));

		dotBuilder.append("}");
		return dotBuilder.toString();
	}

	public Transition getTransition(State state, char letter) {
		if (!states.contains(state)) {
			throw new IllegalArgumentException("Unknown state " + state);
		}
		Map<Character, Transition> m = successorFunction.get(state);
		if (m == null)
			return null;

		Transition t1 = m.get(letter), t2 = m.get(ANY_LETTER);
		if (t1 == null)
			return t2;
		if (t2 == null)
			return t1;
		throw new IllegalStateException(String.format("Non-deterministic transition from state %s with letters '%s'.",
				stateToString(state), charToDot(letter)));
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

		TuringMachine tm = new TuringMachine(alphabet, stateMap.get(initial));
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
			char sourceLetter = toChar(split[1]);
			String successor = split[2];
			char replacement = toChar(split[3]);
			Direction direction = Direction.valueOf(split[4]);

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

			tm.addTransition(q1, sourceLetter, q2, replacement, direction);
		}

		return tm;
	}

	public static enum Direction {
		L, R, N
	}

	public final class Transition {
		public final State successor;
		public final char letter;
		public final Direction direction;

		public Transition(State successor, char letter, Direction direction) {
			this.successor = successor;
			this.letter = letter;
			this.direction = direction;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getEnclosingInstance().hashCode();
			result = prime * result + direction.hashCode();
			result = prime * result + Character.hashCode(letter);
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
			if (!direction.equals(other.direction))
				return false;
			if (letter != other.letter)
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
			return String.format("(%s,%s,%s)", successor, letter, direction);
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

	public class Transition2 {
		State q1, q2;
		char letter, replacement;
		Direction direction;

		public Transition2(State q1, State q2, char letter, char replacement, Direction direction) {
			super();
			this.q1 = q1;
			this.q2 = q2;
			this.letter = letter;
			this.replacement = replacement;
			this.direction = direction;
		}

		public String toString() {
			return String.format("%s;%s;%s;%s;%s", stateToString(q1), letter, stateToString(q2), replacement,
					direction);
		}
	}

	public void checkDeterminism() throws OverlappingTransitionsException {
		for (var e1 : successorFunction.entrySet()) {
			State q = e1.getKey();
			Map<Character, Transition> m = e1.getValue();
			if (m.containsKey(ANY_LETTER)) {
				Optional<Character> cOpt = m.keySet().stream().filter(c -> c != ANY_LETTER).findAny();
				if (!cOpt.isPresent())
					continue;
				char c = cOpt.get();
				Transition t1 = m.get(c), t2 = m.get(ANY_LETTER);
				Transition2 t21 = new Transition2(q, t1.successor, c, t1.letter, t1.direction);
				Transition2 t22 = new Transition2(q, t2.successor, ANY_LETTER, t2.letter, t2.direction);
				throw new OverlappingTransitionsException(t21, t22);
			}
		}
	}

}