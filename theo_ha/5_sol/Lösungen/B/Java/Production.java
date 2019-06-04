import java.util.LinkedList;
import java.util.List;

public class Production {

	public final String lhs, rhs;

	public Production(String lhs, String rhs) {
		this.lhs = lhs;
		this.rhs = rhs;
		for (char c : (lhs + rhs).toCharArray())
			if (!Character.isLetterOrDigit(c))
				throw new IllegalArgumentException("Illegal production: " + lhs + " -> " + rhs);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((lhs == null) ? 0 : lhs.hashCode());
		result = prime * result + ((rhs == null) ? 0 : rhs.hashCode());
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
		Production other = (Production) obj;
		if (lhs == null) {
			if (other.lhs != null)
				return false;
		} else if (!lhs.equals(other.lhs))
			return false;
		if (rhs == null) {
			if (other.rhs != null)
				return false;
		} else if (!rhs.equals(other.rhs))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return lhs + " \u2192 " + rhs;
	}

	private static String strip(String s) {
		String u = "";
		for (char c : s.toCharArray()) {
			if (Character.isWhitespace(c))
				continue;
			if (!Character.isLetterOrDigit(c))
				throw new IllegalArgumentException("Illegal character: '" + c + "'.");
			u += c;
		}
		return u;
	}
        public static Production parse(String s) {
		int i = s.indexOf("->");
		if (i < 0)
			throw new IllegalArgumentException("Illegal production: '" + s + "'");
		String lhs = strip(s.substring(0, i - 1));
		if (lhs.equals("\u03b5"))
			lhs = "";
		String rhs = strip(s.substring(i + 2));
		if (rhs.equals("\u03b5"))
			rhs = "";
		return new Production(lhs, rhs);
	}
	
	public List<String> apply(String w) {
		List<String> results = new LinkedList<>();
		for (int i = 0; i <= w.length() - lhs.length(); i++) {
			if (!lhs.regionMatches(0, w, i, lhs.length()))
				continue;
			String v = w.substring(0, i) + rhs + w.substring(i + lhs.length());
			results.add(v);
		}
		return results;
	}

}
