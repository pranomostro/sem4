import java.util.TreeSet;

public class Single extends Regex {
	
	private final char c;
	
	@Override
	protected int getPrecedence() {
		return 4;
	}
	
	public char getChar() {
		return c;
	}

	public Single(char c) {
		super();
		this.c = c;
	}

	@Override
	protected void appendTo(StringBuilder sb, int prec) {
		sb.append(c);
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + c;
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
		Single other = (Single) obj;
		if (c != other.c)
			return false;
		return true;
	}
	
	public int size() {
		return 1;
	}
	
	public boolean isEmpty() {
		return false;
	}
	
	public boolean isTrivial() {
		return false;
	}
	
	@Override
	public TreeSet<String> enumerateWords() {
		TreeSet<String> words = new TreeSet<String>();
		words.add(c + "");
		return words;
	}
	
}
