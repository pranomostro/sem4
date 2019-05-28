
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
		Regex r = parseNonAlternative();
		char c = peek();
		if (c == EOF || c == ')')
			return r;
		if (c == '|') {
			pos++;
			Regex r2 = parseAlternative();
			return new Alternative(r, r2);
		}
		throw new IllegalArgumentException(
				String.format("Expected '|' or end of input, but got %s instead at position %d.", prettyPeek(), pos));
	}

	private Regex parseNonAlternative() {
		char c = peek();

		if (c == EOF || c == ')' || c == '|')
			return Regex.EPSILON;

		Regex r = null;
		do {
			Regex r2;
			if (c == '∅') {
				r2 = Regex.EMPTY;
				pos++;
		    } else if (c == '{') {
		        pos++;
		        if (peek() != '}')
					throw new IllegalArgumentException(
							String.format("Expected '}', but got %s instead at position %d.", prettyPeek(), pos));
			    pos++;
		        r2 = Regex.EMPTY;
			} else if (c == 'ε') {
				r2 = Regex.EPSILON;
				pos++;
			} else if (c == '(') {
				pos++;
				r2 = parseAlternative();
				if (peek() != ')') {
					throw new IllegalArgumentException(
							String.format("Expected ')', but got %s instead at position %d.", prettyPeek(), pos));
				}
				pos++;
			} else if (isSymbol(c)) {
				r2 = new Single(c);
				pos++;
			} else if (c == '*') {
				r2 = new Star(Regex.EPSILON);
				pos++;
			} else {
				throw new IllegalArgumentException(String.format(
						"Expected alphabet symbol or one of '(', '|', ')', '*', but got %s instead at position %d.", prettyPeek(), pos));
			}

			while (peek() == '*') {
				r2 = new Star(r2);
				pos++;
			}

			if (r == null)
				r = r2;
			else
				r = new Concat(r, r2);

			c = peek();
		} while (c != EOF && c != ')' && c != '|');

		return r;
	}

}
