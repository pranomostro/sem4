import java.util.ArrayDeque;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Queue;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;

public class ContextSensitiveGrammar implements Iterable<Production> {

    public static boolean isTerminal(char c) {
        return (Character.isLetterOrDigit(c) && Character.isLowerCase(c));
    }

    public static boolean isNonTerminal(char c) {
        return Character.isUpperCase(c);
    }


	private final char startSymbol;
	private final Set<Production> productions = new HashSet<>();

	public ContextSensitiveGrammar(char startSymbol, Collection<Production> productions) {
		this.startSymbol = startSymbol;
		this.productions.addAll(productions);

		Production epsilonProduction = new Production(startSymbol + "", "");
		boolean containsEpsilon = productions.contains(epsilonProduction);
		for (Production p : productions)
			if (p.rhs.length() < p.lhs.length() && !p.equals(epsilonProduction)
					|| containsEpsilon && p.rhs.contains("" + startSymbol))
				throw new IllegalArgumentException("Non-context-sensitive production: " + p);
	}

	public char getStartSymbol() {
		return startSymbol;
	}

	public Set<Production> getProductions() {
		return new HashSet<>(productions);
	}

	@Override
	public Iterator<Production> iterator() {
		return new ReadOnlyIterator<>(productions.iterator());
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("CSG\n");
		sb.append("Start symbol: " + startSymbol + "\n");
		sb.append("Productions:\n");
		for (Production p : productions)
			sb.append(p + "\n");
		return sb.toString();
	}

	private static String expect(Scanner sc, String s, boolean colon) {
		String prefix = (colon) ? s + ":" : s;
		if (!sc.hasNextLine())
			throw new IllegalArgumentException("Expected '" + prefix + "', but got end of input instead.");
		String line = sc.nextLine().trim();
		if (!line.startsWith(prefix))
			throw new IllegalArgumentException("Expected '" + prefix + "', but got '" + line + "' instead.");
		if (colon) {
			return line.substring(prefix.length()).trim();
		} else {
			return null;
		}
	}

	public static ContextSensitiveGrammar parse(Scanner sc) {
		expect(sc, "CSG", false);
		String startSymbol = expect(sc, "Start symbol", true);
		if (startSymbol.length() != 1 || !Character.isLetterOrDigit(startSymbol.charAt(0)))
			throw new IllegalArgumentException("Illegal start symbol '" + startSymbol + "'.");

		expect(sc, "Productions:", false);
		Set<Production> productions = new HashSet<>();
		String line;
		while (sc.hasNextLine() && !(line = sc.nextLine()).contentEquals("END"))
			productions.add(Production.parse(line));
		return new ContextSensitiveGrammar(startSymbol.charAt(0), productions);
	}

	public Set<String> enumerateWordsRaw(int maxLength) {
		Set<String> s = new HashSet<>();
		Queue<String> workList = new ArrayDeque<>();

		s.add(Character.toString(startSymbol));
		workList.offer(Character.toString(startSymbol));

		while (!workList.isEmpty()) {
			String w = workList.poll();
			for (Production p : productions) {
				p.apply(w).stream().filter(u -> u.length() <= maxLength && !s.contains(u)).forEach(u -> {
					s.add(u);
					workList.offer(u);
				});
			}
		}
		return s;
	}

	private static final boolean isTerminalWord(String w) {
		for (char c : w.toCharArray())
			if (!Character.isLowerCase(c))
				return false;
		return true;
	}

	public Set<String> enumerateWords(int maxLength) {
		return enumerateWordsRaw(maxLength).stream().filter(x -> isTerminalWord(x)).collect(Collectors.toSet());
	}

        public static final void printWord(String w) {
		System.out.println("\"" + w + "\"");
	}

	public static int compareWords(String s1, String s2) {
		int cmp = s1.length() - s2.length();
		if (cmp != 0)
			return cmp;
		return s1.compareTo(s2);
	}


        public static void main(String[] args) {
                Scanner sc = new Scanner(System.in);
                boolean raw;
                {
                  String mode = sc.nextLine();
                  if (mode.equals("Raw")) {
                          raw = true;
                  } else if (mode.equals("Normal")) {
                          raw = false;
                  } else {
                          System.err.println("Illegal mode: " + mode);
                          return;
                  }
                }
                                
                while (sc.hasNextLine()) {
                        ContextSensitiveGrammar g = ContextSensitiveGrammar.parse(sc);
                        int l = Integer.parseInt(sc.nextLine());
                        Set<String> words = (raw) ? g.enumerateWordsRaw(l) : g.enumerateWords(l);
                        words.stream().sorted((x, y) -> compareWords(x, y)).forEach(x -> printWord(x));
                        System.out.println("END");
                }
                sc.close();
        }
}
