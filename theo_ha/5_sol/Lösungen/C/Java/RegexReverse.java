

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class RegexReverse implements Regex.Visitor<Regex> {

	@Override
	public Regex visitEmpty() {
		return Regex.empty();
	}

	@Override
	public Regex visitEpsilon() {
		return Regex.epsilon();
	}

	@Override
	public Regex visitSingle(char c) {
		return Regex.single(c);
	}

	@Override
	public Regex visitAlternative(Set<Regex> rs) {
		return Regex.alternatives(rs.stream().map(x -> x.accept(this)));
	}

	@Override
	public Regex visitConcat(List<Regex> rs) {
		LinkedList<Regex> rs2 = new LinkedList<>();
		for (Regex r : rs)
			rs2.addFirst(r.accept(this));
		return Regex.concats(rs2);
	}

	@Override
	public Regex visitStar(Regex r) {
		return Regex.star(r.accept(this));
	}
	
	private static final RegexReverse inst = new RegexReverse();
	
	public static final Regex reverseRegex(Regex r) {
		return r.accept(inst);
	}

}
