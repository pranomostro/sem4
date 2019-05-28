import java.util.TreeSet;

public class Epsilon extends Regex {

	private Epsilon() {
	}
	
	private static final Epsilon inst = new Epsilon();
	
	public static final Epsilon getInstance() {
		return inst;
	}
	
	@Override
	protected void appendTo(StringBuilder sb, int prec) {
      sb.append("ε");		
	}

	@Override
	protected int getPrecedence() {
		return 4;
	}
	
	@Override
	public int hashCode() {
		return 47;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		return (getClass() == obj.getClass());
	}
	
	public int size() {
		return 1;
	}
	
	public boolean isEmpty() {
		return false;
	}
	
	public boolean isTrivial() {
		return true;
	}
	
	@Override
	public TreeSet<String> enumerateWords() {
		TreeSet<String> words = new TreeSet<String>();
		words.add("");
		return words;
	}
	
}
