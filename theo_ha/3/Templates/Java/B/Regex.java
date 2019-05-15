import java.util.TreeSet;
import java.util.Set;

public abstract class Regex {

	protected abstract void appendTo(StringBuilder sb, int prec);
	protected abstract int getPrecedence();

	protected final void toStringAux(StringBuilder sb, int prec) {
		if (prec > getPrecedence()) {
			sb.append("(");
			appendTo(sb, getPrecedence());
			sb.append(")");
		} else {
		  appendTo(sb, getPrecedence());
		}
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		toStringAux(sb, 0);
		return sb.toString();
	}

	public static final Empty EMPTY = Empty.getInstance();
	public static final Epsilon EPSILON = Epsilon.getInstance();
	
	public abstract int size();
	
	public abstract EpsilonNFA toEpsilonNFA(Set<Character> alphabet);
	
	public static Regex parse(String s) {
		return (new RegexParser(s)).parse();
	}
	
}
