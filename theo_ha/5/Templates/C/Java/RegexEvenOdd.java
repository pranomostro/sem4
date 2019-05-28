

import java.util.List;
import java.util.Set;

public class RegexEvenOdd implements Regex.Visitor<RegexEvenOdd.EvenOdd> {

	public static final class EvenOdd {
		public final boolean even, odd;

		public EvenOdd(boolean even, boolean odd) {
			this.even = even;
			this.odd = odd;
		}

		@Override
		public int hashCode() {
			return (even ? 1 : 0) | (odd ? 2 : 0);
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			EvenOdd other = (EvenOdd) obj;
			if (even != other.even)
				return false;
			if (odd != other.odd)
				return false;
			return true;
		}

		@Override
		public String toString() {
			return "EvenOdd [even=" + even + ", odd=" + odd + "]";
		}
		
	}

	@Override
	public EvenOdd visitEmpty() {
		return new EvenOdd(true, true);
	}

	@Override
	public EvenOdd visitEpsilon() {
		return new EvenOdd(true, false);
	}

	@Override
	public EvenOdd visitSingle(char c) {
		return new EvenOdd(false, true);
	}

	private static final EvenOdd evenOddAlternative(EvenOdd x, EvenOdd y) {
		return new EvenOdd(x.even && y.even, x.odd && y.odd);
	}
	
	@Override
	public EvenOdd visitAlternative(Set<Regex> rs) {
		EvenOdd dflt = new EvenOdd(true, true);
		return rs.stream().
				  map(r -> r.accept(this)).
				  reduce(dflt, (x, y) -> evenOddAlternative(x, y));
	}
	
	private static final EvenOdd evenOddConcat(EvenOdd x, EvenOdd y) {
		return new EvenOdd(x.even && y.even || x.odd && y.odd,
				           x.odd && y.even  || x.even && y.odd);
	}

	@Override
	public EvenOdd visitConcat(List<Regex> rs) {
		EvenOdd dflt = new EvenOdd(true, false);
		return rs.stream().
				  map(r -> r.accept(this)).
				  reduce(dflt, (x, y) -> evenOddConcat(x, y));
	}

	@Override
	public EvenOdd visitStar(Regex r) {
		EvenOdd x = r.accept(this);
		return new EvenOdd(x.even, false);
	}
	
	private static final RegexEvenOdd inst = new RegexEvenOdd();
	
	public static final EvenOdd isRegexEvenOdd(Regex r) {
		return r.accept(inst);
	}
	
	public static final boolean isRegexEven(Regex r) {
		return isRegexEvenOdd(r).even;
	}
	
	public static final boolean isRegexOdd(Regex r) {
		return isRegexEvenOdd(r).odd;
	}

}
