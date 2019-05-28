import java.util.TreeSet;

public class Alternative extends Regex {
	
	private final Regex r1, r2;
	private final int size;
	
	public Regex getR1() {
		return r1;
	}

	public Regex getR2() {
		return r2;
	}
	
	public Alternative(Regex r1, Regex r2) {
		super();
		this.r1 = r1;
		this.r2 = r2;
		this.size = r1.size() + r2.size() + 1;
	}

	@Override
	protected void appendTo(StringBuilder sb, int prec) {
		r1.toStringAux(sb, getPrecedence());
		sb.append('|');
		r2.toStringAux(sb, getPrecedence());
	}

	@Override
	protected int getPrecedence() {
		return 0;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((r1 == null) ? 0 : r1.hashCode());
		result = prime * result + ((r2 == null) ? 0 : r2.hashCode());
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
		Alternative other = (Alternative) obj;
		if (r1 == null) {
			if (other.r1 != null)
				return false;
		} else if (!r1.equals(other.r1))
			return false;
		if (r2 == null) {
			if (other.r2 != null)
				return false;
		} else if (!r2.equals(other.r2))
			return false;
		return true;
	}
	
	public int size() {
		return size;
	}
	
	public boolean isEmpty() {
		return r1.isEmpty() && r2.isEmpty();
	}
	
	public boolean isTrivial() {
		return r1.isTrivial() && r2.isTrivial();
	}
	
	@Override
	public TreeSet<String> enumerateWords() {
		TreeSet<String> words1 = r1.enumerateWords();
		if (words1 == null) return null;
		TreeSet<String> words2 = r2.enumerateWords();
		if (words2 == null) return null;
		words1.addAll(words2);
		return words1;
	}
	
}
