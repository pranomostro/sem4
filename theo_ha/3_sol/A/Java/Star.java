import java.util.TreeSet;

public class Star extends Regex {

	private final Regex r;
	private final int size;
	
	public Regex getRegex() {
		return r;
	}
	
	public Star(Regex r) {
		super();
		this.r = r;
		this.size = r.size() + 1;
	}
	
	@Override
	protected int getPrecedence() {
		return 2;
	}

	@Override
	protected void appendTo(StringBuilder sb, int prec) {
		r.toStringAux(sb, getPrecedence());
		sb.append('*');
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((r == null) ? 0 : r.hashCode());
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
		Star other = (Star) obj;
		if (r == null) {
			if (other.r != null)
				return false;
		} else if (!r.equals(other.r))
			return false;
		return true;
	}
	
	public int size() {
		return size;
	}
	
	public boolean isEmpty() {
		return false;
	}
	
	public boolean isTrivial() {
		return r.isTrivial();
	}
	
	@Override
	public TreeSet<String> enumerateWords() {
		if (!r.isTrivial()) return null;
		TreeSet<String> words = new TreeSet<String>();
		words.add("");
		return words;
	}
	
}
