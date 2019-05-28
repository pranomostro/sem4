import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
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
	
	public EpsilonNFA toEpsilonNFA(Set<Character> alpha) {
	    State start = new State();
		return new EpsilonNFA(Collections.singleton(start), new HashSet<Transition>(), alpha, start, Collections.singleton(start));
	}
	
}
