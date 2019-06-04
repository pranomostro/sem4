
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

public abstract class Regex {
	
	private static final class ReadOnlyIterator<T> implements Iterator<T> {

		private final Iterator<T> it;
		
		public ReadOnlyIterator(Iterator<T> it) {
			this.it = it;
		}
		
		@Override
		public boolean hasNext() {
			return it.hasNext();
		}

		@Override
		public T next() {
			return it.next();
		}
		
	}
	
	public static final class Empty extends Regex {
		
		@Override
		protected void appendTo(StringBuilder sb, int prec) {
	      sb.append("{}");
		}
		
		private Empty() {
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

		@Override
		public boolean isNullable() {
			return false;
		}

		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitEmpty();
		}
		
	}
	
	public static final class Epsilon extends Regex {

		private Epsilon() {
		}
		
		@Override
		protected void appendTo(StringBuilder sb, int prec) {
	      sb.append("()");		
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

		@Override
		public boolean isNullable() {
			return true;
		}
		
		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitEpsilon();
		}
		
	}

	public static final class Single extends Regex {
		
		private final char c;
		
		@Override
		protected int getPrecedence() {
			return 4;
		}
		
		public char getChar() {
			return c;
		}

		private Single(char c) {
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

		@Override
		public boolean isNullable() {
			return false;
		}
		
		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitSingle(c);
		}
		
	}

	public static final class Concat extends Regex implements Iterable<Regex> {

		private final List<Regex> children;
		private final int size;
		private final boolean nullable;
		
		private Concat(List<Regex> children) {
			super();
			List<Regex> children2 = new LinkedList<>();
			int size = 1;
			boolean nullable = true;
			for (Regex r : children) {
				if (r instanceof Epsilon) continue;
				children2.add(r);
				size += r.size();
				nullable = nullable && r.isNullable();
			}
			this.size = size;
			this.nullable = nullable;
			this.children = Collections.unmodifiableList(children2);
		}
		
		public List<Regex> getChildren() {
			return children;
		}
		
		@Override
		protected void appendTo(StringBuilder sb, int prec) {
			for (Regex r : children)
				r.toStringAux(sb, getPrecedence());			
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((children == null) ? 0 : children.hashCode());
			result = prime * result + size;
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
			Concat other = (Concat) obj;
			if (children == null) {
				if (other.children != null)
					return false;
			} else if (!children.equals(other.children))
				return false;
			if (size != other.size)
				return false;
			return true;
		}

		@Override
		protected int getPrecedence() {
			return 1;
		}
		
		public int size() {
			return size;
		}

		@Override
		public boolean isNullable() {
			return nullable;
		}

		@Override
		public Iterator<Regex> iterator() {
			return new ReadOnlyIterator<Regex>(children.iterator());
		}
		
		public Stream<Regex> stream() {
			return children.stream();
		}
		
		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitConcat(children);
		}
		
	}
	
	public static final class Alternative extends Regex implements Iterable<Regex> {
		
		private final Set<Regex> children;
		private final int size;
		private final boolean nullable;
		
		private Alternative(Iterable<Regex> children) {
			super();
			Set<Regex> children2 = new HashSet<Regex>();
			int size = 1;
			boolean nullable = false;
			for (Regex r : children) {
				if (r instanceof Empty) continue;
				children2.add(r);
				size += r.size();
				nullable = nullable || r.isNullable();
			}
			this.children = Collections.unmodifiableSet(children2);
			this.size = size;
			this.nullable = nullable;
		}
		
		public Set<Regex> getChildren() {
			return children;
		}
		
		@Override
		protected void appendTo(StringBuilder sb, int prec) {
			boolean first = true;
			for (Regex r : children) {
				if (!first) {
					sb.append("|");
				} else {
					first = false;
				}
				r.toStringAux(sb, getPrecedence());			
			}
		}

		@Override
		protected int getPrecedence() {
			return 0;
		}
		
		public int size() {
			return size;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((children == null) ? 0 : children.hashCode());
			result = prime * result + size;
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
			if (children == null) {
				if (other.children != null)
					return false;
			} else if (!children.equals(other.children))
				return false;
			if (size != other.size)
				return false;
			return true;
		}

		@Override
		public boolean isNullable() {
			return nullable;
		}

		@Override
		public Iterator<Regex> iterator() {
			return new ReadOnlyIterator<Regex>(children.iterator());
		}
		
		public Stream<Regex> stream() {
			return children.stream();
		}
		
		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitAlternative(children);
		}
		
	}
	
	public static final class Star extends Regex {

		private final Regex r;
		private final int size;
		
		public Regex getRegex() {
			return r;
		}
		
		private Star(Regex r) {
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

		@Override
		public boolean isNullable() {
			return true;
		}
		
		@Override
		public <R> R accept(Visitor<R> vis) {
			return vis.visitStar(r);
		}
		
	}
	
	/**
	 * Visitor interface for regular expressions, with result type R. 
	 */
	public static interface Visitor<R> {
		public R visitEmpty();
		public R visitEpsilon();
		public R visitSingle(char c);
		public R visitAlternative(Set<Regex> rs);
		public R visitConcat(List<Regex> rs);
		public R visitStar(Regex r);
	}


	protected abstract void appendTo(StringBuilder sb, int prec);

	protected abstract int getPrecedence();
	
	public abstract <R> R accept(Visitor<R> vis);

	protected final void toStringAux(StringBuilder sb, int prec) {
		if (prec > getPrecedence()) {
			sb.append("(");
			appendTo(sb, getPrecedence());
			sb.append(")");
		} else {
			appendTo(sb, getPrecedence());
		}
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		toStringAux(sb, 0);
		return sb.toString();
	}


	
	// Smart constructors
	private static final Empty EMPTY = new Empty();
	private static final Epsilon EPSILON = new Epsilon();
	private static final Map<Character, Single> singleInsts = new HashMap<>();
	
	public static final Regex empty() {
		return EMPTY;
	}

	public static final Regex epsilon() {
		return EPSILON;
	}
	
	public static final Regex single(char c) {
		Single r = singleInsts.get(c);
		if (r == null) {
			r = new Single(c);
			singleInsts.put(c, r);
		}
		return r;
	}

	public final Regex alternative(Iterator<Regex> it) {
		Set<Regex> s = new HashSet<Regex>();

		boolean nullable = false, hasEpsilon = false;
		Regex r = this;

		do {
			if (r instanceof Epsilon) {
				hasEpsilon = true;
			} else {
				nullable = nullable || r.isNullable();
				if (r instanceof Alternative) {
					for (Regex r2 : (Alternative) r)
						s.add(r2);
				} else {
					if (!(r instanceof Empty)) s.add(r);
				}
			}
			r = (it.hasNext()) ? it.next() : null;
		} while (r != null);
		
		if (hasEpsilon && !nullable)
			s.add(Regex.epsilon());

		if (s.size() == 0)
			return empty();
		else if (s.size() == 1)
			return s.iterator().next();
		else
			return new Alternative(s);
	}
	
	public final Regex alternative(Iterable<Regex> rs) {
		return alternative(rs.iterator());
	}

	public final Regex alternative(Stream<Regex> rs) {
		return alternative(rs.iterator());
	}
	
	public final Regex alternative(Regex... rs) {
		return alternative(Arrays.stream(rs));
	}
	
	public static final Regex alternatives(Iterator<Regex> it) {
		if (it.hasNext())
			return it.next().alternative(it);
		else
			return Regex.empty();
	}

	public static final Regex alternatives(Iterable<Regex> rs) {
		return alternatives(rs.iterator());
	}

	public static final Regex alternatives(Stream<Regex> rs) {
		return alternatives(rs.iterator());
	}
	
	public static final Regex alternatives(Regex... rs) {
		return alternatives(Arrays.stream(rs));
	}
	

	public final Regex concat(Iterator<Regex> it) {
		List<Regex> l = new LinkedList<Regex>();
		Regex r = this;
		do {
			if (r instanceof Empty) {
				return empty();
			} else if (r instanceof Concat) {
				for (Regex r2 : (Concat) r)
					l.add(r2);
			} else {
				if (!(r instanceof Epsilon)) l.add(r);
			}
			r = (it.hasNext()) ? it.next() : null;
		} while (r != null);

		if (l.size() == 0)
			return epsilon();
		else if (l.size() == 1)
			return l.iterator().next();
		else
			return new Concat(l);
	}
	
	public final Regex concat(Stream<Regex> rs) {
		return concat(rs.iterator());
	}
	
	public final Regex concat(Iterable<Regex> rs) {
		return concat(rs.iterator());
	}
	
	public final Regex concat(Regex... rs) {
		return concat(Arrays.stream(rs));
	}
	
	public static final Regex concats(Iterator<Regex> it) {
		if (it.hasNext())
			return it.next().concat(it);
		else
			return Regex.epsilon();
	}

	public static final Regex concats(Stream<Regex> rs) {
		return concats(rs.iterator());
	}
	
	public static final Regex concats(Iterable<Regex> rs) {
		return concats(rs.iterator());
	}
	
	public static final Regex concats(Regex... rs) {
		return concats(Arrays.stream(rs));
	}
	
	
	public static final Regex star(Regex r) {
		if (r instanceof Star)
			return r;
		else if (r instanceof Empty || r instanceof Epsilon)
			return epsilon();
		else
			return new Star(r);
	}
	
	public Regex star() {
		return Regex.star(this);
	}
	
	
	// Utility functions
	public abstract int size();

	/**
	 * Returns true iff the regular expression generates the empty word.
	 */
	public abstract boolean isNullable();
	
	public static final Regex parse(String s) {
		return (new RegexParser(s)).parse();
	}
	
	public Set<Character> getAlphabet() {
		final Set<Character> alphabet = new HashSet<Character>();
		var visitor = new Visitor<Void>() {
			public Void visitEmpty() {
				return null;
			}

			public Void visitEpsilon() {
				return null;
			}

			public Void visitSingle(char c) {
				alphabet.add(c);
				return null;
			}

			@Override
			public Void visitAlternative(Set<Regex> rs) {
				rs.stream().forEach(r -> r.accept(this));
				return null;
			}

			@Override
			public Void visitConcat(List<Regex> rs) {
				rs.stream().forEach(r -> r.accept(this));
				return null;
			}

			@Override
			public Void visitStar(Regex r) {
				r.accept(this);
				return null;
			}
		};
		accept(visitor);
		return alphabet;
	}
	
}
