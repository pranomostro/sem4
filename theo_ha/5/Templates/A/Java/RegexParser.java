import java.util.LinkedList;
import java.util.List;

public class RegexParser {

	private static final char EOF = 0;
	char[] input;
	int pos;

	private boolean eof() {
		return pos >= input.length;
	}

	private char peek() {
		while (pos < input.length && Character.isWhitespace(input[pos]))
			pos++;
		if (pos >= input.length)
			return EOF;
		else
			return input[pos];
	}

	private String prettyPeek() {
		char c = peek();
		if (c == EOF)
			return "end of input";
		else
			return "'" + c + "'";
	}

	@SuppressWarnings("unused")
	private RegexParser() {
	}

	public RegexParser(String s) {
		input = s.toCharArray();
	}

	public static final boolean isSymbol(char c) {
		return Character.isAlphabetic(c) || Character.isDigit(c);
	}

	public Regex parse() {
		Regex r = parseAlternative();
		if (!eof()) {
			throw new IllegalArgumentException(
					String.format("Expected end of input, but got %s instead at position %d.", prettyPeek(), pos));
		}
		return r;
	}

	private Regex parseAlternative() {
		List<Regex> l = new LinkedList<Regex>();
		l.add(parseNonAlternative());

		char c = peek();
		while (c == '|') {
			pos++;
		    l.add(parseNonAlternative());
			c = peek();
		}
		
		if (c != EOF && c != ')') {
			throw new IllegalArgumentException(
					String.format("Expected '|' or end of input, but got %s instead at position %d.", prettyPeek(), pos));
		}
		
		return Regex.alternatives(l);
	}

	private Regex parseNonAlternative() {
		char c = peek();

		if (c == EOF || c == ')' || c == '|')
			return Regex.epsilon();

		List<Regex> l = new LinkedList<Regex>();
		do {
			Regex r;
			if (c == '{') {
				pos++;
				if (peek() != '}') {
					throw new IllegalArgumentException(
							String.format("Expected '}', but got %s instead at position %d.", prettyPeek(), pos));
				}
				pos++;
				r = Regex.empty();
			} else if (c == '∅') {
				r = Regex.empty();
				pos++;
			} else if (c == 'ε' || c == '*') {
				// == '*' because of "(*)", which corresponds to "(ε*)"
				r = Regex.epsilon();
				pos++;
			} else if (c == '(') {
				pos++;
				r = parseAlternative();
				if (peek() != ')') {
					throw new IllegalArgumentException(
							String.format("Expected ')', but got %s instead at position %d.", prettyPeek(), pos));
				}
				pos++;
			} else if (isSymbol(c)) {
				r = Regex.single(c);
				pos++;
			} else {
				throw new IllegalArgumentException(String.format(
						"Expected alphabet symbol or one of '(', '|', ')', '*', but got %s instead at position %d.", prettyPeek(), pos));
			}

			while (peek() == '*') {
				r = Regex.star(r);
				pos++;
			}

			l.add(r);
			c = peek();
		} while (c != EOF && c != ')' && c != '|');

		return Regex.concats(l);
	}

}
