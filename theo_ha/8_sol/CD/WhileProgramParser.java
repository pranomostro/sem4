import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class WhileProgramParser {
	
	public static class ParserException extends Error {
		public final int line;
		public final String message;
		
		public ParserException(String message, int line) {
			super();
			this.line = line;
			this.message = message;
		}

		public String getMessage() {
			return message + " at line " + line;
		}
		
		public ParserException offset(int offset) {
			return new ParserException(message, line + offset);
		}
		
	}

	private static abstract class Token {
		final String content;
		final int line;

		public Token(String content, int line) {
			super();
			this.content = content;
			this.line = line;
		}

		public String toString() {
			return String.format("'%s'", content);
		}
	}

	private static class ReservedToken extends Token {

		public ReservedToken(String content, int line) {
			super(content, line);
		}

	}

	private static class NumeralToken extends Token {
		final int n;

		public NumeralToken(String content, int line, int n) {
			super(content, line);
			this.n = n;
		}

	}

	private static class VariableToken extends Token {
		final int var;

		public VariableToken(String content, int line, int var) {
			super(content, line);
			this.var = var;
		}

	}
	
	private static class EOFToken extends Token {
		
		public EOFToken(int line) {
			super("EOF", line);
		}
		
		public String toString() {
			return "end of input";
		}
		
	}

	private static class Tokenizer {

		final char buf[];
		final LinkedList<Token> tokens = new LinkedList<>();
		int index = 0, line = 1;

		public Tokenizer(String w) {
			buf = (w + "\0").toCharArray();
			skipWhiteSpace();
			while (index < w.length()) {
				step();
				skipWhiteSpace();
			}
			tokens.add(new EOFToken(line));
		}

		private char peek() {
			return buf[index];
		}

		private String prettyPeek() {
			char c = peek();
			if (c == '\0')
				return "end of input";
			else
				return "'" + c + "'";
		}

		private char poll() {
			char c = peek();
			index++;
			if (c == '\r') {
				line++;
				if (peek() == '\n')
					index++;
			} else if (c == '\n') {
				line++;
			}
			return c;
		}

		private static boolean isDelimiter(char c) {
			return !(Character.isLetterOrDigit(c));
		}

		private boolean consume(String s) {
			if (index + s.length() >= buf.length)
				return false;
			char next = buf[index + s.length()];
			if (Character.isLetterOrDigit(s.charAt(s.length() - 1)) && !isDelimiter(next))
				return false;
			for (int i = 0; i < s.length(); i++)
				if (Character.toLowerCase(buf[index + i]) != Character.toLowerCase(s.charAt(i)))
					return false;
			tokens.add(new ReservedToken(s, line));
			for (int i = 0; i < s.length(); i++)
				poll();
			return true;
		}

		private boolean consume(String... ss) {
			for (String s : ss)
				if (consume(s))
					return true;
			return false;
		}

		private void skipWhiteSpace() {
			while (Character.isWhitespace(peek()))
				poll();
		}
		
		private String getRestOfLine() {
			if (peek() == '\0')
				return "end of input";
			StringBuilder sb = new StringBuilder();
			sb.append("'");
			int i = index;
			while (buf[i] != '\0' && buf[i] != '\r' && buf[i] != '\n')
				sb.append(buf[i++]);
			sb.append("'");
			return sb.toString();
		}
		
		private void step() {
			if (consume("WHILE", "DO", "IF", "THEN", "ELSE", "END", ";", "!=", "=", "+", "-", ":="))
				return;

			if (peek() == 'x' && Character.isDigit(buf[index + 1])) {
				poll();
				StringBuilder s = new StringBuilder();
				while (Character.isDigit(peek()))
					s.append(poll());
				if (isDelimiter(peek())) {
					tokens.add(new VariableToken("x" + s.toString(), line, Integer.parseInt(s.toString())));
					return;
				}
			}

			if (Character.isDigit(peek())) {
				StringBuilder s = new StringBuilder();
				while (Character.isDigit(peek()))
					s.append(poll());
				if (isDelimiter(peek())) {
					tokens.add(new NumeralToken(s.toString(), line, Integer.parseInt(s.toString())));
					return;
				}
			}

			throw new ParserException(
					String.format("Tokenization error at %s", getRestOfLine()), line);
		}

		public static LinkedList<Token> tokenize(String s) {
			Tokenizer t = new Tokenizer(s);
			return t.tokens;
		}

	}

	LinkedList<Token> tokens;

	public boolean isEOF() {
		return tokens.peek() instanceof EOFToken;
	}
	
	public WhileProgramParser(String s) {
		tokens = Tokenizer.tokenize(s);
	}

	private boolean reserved(String s) {
		Token tok = tokens.peek();
		if (tok instanceof ReservedToken && ((ReservedToken) tok).content.toLowerCase().equals(s.toLowerCase())) {
			tokens.poll();
			return true;
		} else {
			return false;
		}
	}

	private void reservedStrict(String s) {
		if (!reserved(s))
			expectedButGot(s);
	}

	private int variable() {
		Token tok = tokens.peek();
		if (tok instanceof VariableToken) {
			tokens.poll();
			return ((VariableToken) tok).var;
		} else {
			return -1;
		}
	}

	private int variableStrict() {
		int i = variable();
		if (i < 0)
			expectedButGot("variable name");
		return i;
	}

	private int numeral() {
		Token tok = tokens.peek();
		if (tok instanceof NumeralToken) {
			tokens.poll();
			return ((NumeralToken) tok).n;
		} else {
			expectedButGot("numeral");
		}
		return -1;
	}

	private void expectedButGot(String exp) {
		throw new ParserException(String.format("Expected '%s' but got %s.", exp, tokens.peek()), tokens.peek().line);
	}

	private WhileProgram parseAssignment() {
		int i = variable();
		if (i < 0)
			return null;
		reservedStrict(":=");
		int j = variableStrict();
		if (reserved("+")) {
			return new WhileProgram.Increment(i, j, numeral());
		} else if (reserved("-")) {
			return new WhileProgram.Decrement(i, j, numeral());
		} else {
			expectedButGot("'+' or '-'");
			return null;
		}
	}

	private void zero() {
		Token tok = tokens.peek();
		if (tok instanceof NumeralToken && ((NumeralToken) tok).n == 0) {
			tokens.poll();
		} else {
			expectedButGot("0");
		}
	}

	private WhileProgram parseIf() {
		if (!reserved("IF"))
			return null;
		int testVar = variableStrict();
		reservedStrict("=");
		zero();
		reservedStrict("THEN");
		WhileProgram thenBody = parseAux1();
		reservedStrict("ELSE");
		WhileProgram elseBody = parseAux1();
		reservedStrict("END");
		return new WhileProgram.IfThenElse(testVar, thenBody, elseBody);
	}

	private WhileProgram parseWhile() {
		if (!reserved("WHILE"))
			return null;
		int testVar = variableStrict();
		reservedStrict("!=");
		zero();
		reservedStrict("DO");
		WhileProgram loopBody = parseAux1();
		reservedStrict("END");
		return new WhileProgram.While(testVar, loopBody);
	}

	private WhileProgram parseEmptyBlock() {
		return new WhileProgram.Block(Collections.emptyList());
	}
	
	private WhileProgram parseAux2() {
		WhileProgram p;
		if ((p = parseAssignment()) != null)
			return p;
		else if ((p = parseIf()) != null)
			return p;
		else if ((p = parseWhile()) != null)
			return p;
		else if ((p = parseEmptyBlock()) != null)
			return p;
		return null;
	}

	private WhileProgram parseAux1() {
		WhileProgram p = parseAux2();
		List<WhileProgram> l = new LinkedList<>();
		l.add(p);
		while (reserved(";")) {
			l.add(parseAux2());
		}
		if (l.size() == 1)
			return p;
		else
			return new WhileProgram.Block(l);
	}
	
	public WhileProgram parse() {
		WhileProgram p = parseAux1();
		if (!isEOF())
			expectedButGot("';' or end of input");
		return p;
	}

}
