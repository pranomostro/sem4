import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

public class Empty extends Regex {
	
	@Override
	protected void appendTo(StringBuilder sb, int prec) {
      sb.append("∅");
	}
	
	private static final Empty inst = new Empty();
	
	public static final Empty getInstance() {
		return inst;
	}

	@Override
	protected int getPrecedence() {
		return 4;
	}
	
	@Override
	public int hashCode() {
		return 23;
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
		return new EpsilonNFA(Collections.singleton(start), new HashSet<Transition>(), alpha, start, new HashSet<State>());
	}
	
}
