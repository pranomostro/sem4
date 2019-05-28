import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;
import java.util.HashSet;

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
	
	public EpsilonNFA toEpsilonNFA(Set<Character> alpha) {
		Set<State> states = new HashSet<>();
		
	    State start = new State();
	    State end = new State();
	    Transition t = new Transition(start,end,c);
	    
	    states.add(start);
	    states.add(end);
		return new EpsilonNFA(states, Collections.singleton(t), alpha, start, Collections.singleton(end));
	}
	
}
