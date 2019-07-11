import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public abstract class WhileProgram {
	
	public interface Visitor<A> {
		
		public A visitIncrement(int i, int j, int amount);
		public A visitDecrement(int i, int j, int amount);
		public A visitBlock(List<WhileProgram> parts);
		public A visitIfThenElse(int testVar, WhileProgram thenBody, WhileProgram elseBody);
		public A visitWhile(int testVar, WhileProgram loopBody);
		
	}
	
	public static class State {
		
		private final Map<Integer, Integer> vars;
		private int fuel;
		
		public State(Map<Integer, Integer> vars, int fuel) {
			this.vars = new HashMap<>(vars);
			this.fuel = fuel;
		}
		
		private static final void ensureNonNegative(int var) {
			if (var < 0)
				throw new IllegalArgumentException("Illegal variable: " + var);
		}
		
		public int get(int var) {
			ensureNonNegative(var);
			Integer result = vars.get(var);
			return (result == null) ? 0 : result;
		}
		
		public void set(int var, int value) {
			ensureNonNegative(var);
			vars.put(var,  value);
		}
		
		public String toString() {
			StringBuilder sb = new StringBuilder();
			vars.keySet().stream().sorted().forEach(var -> sb.append(var).append(" = ").append(vars.get(var)).append("\n"));
			return sb.toString();
		}
		
		public int getFuel() {
			return fuel;
		}
		
		public void tick() throws OutOfFuelException {
			if (fuel == 0)
				throw new OutOfFuelException();
			else
				fuel--;
		}
		
		public static State of(int fuel, int... input) {
			Map<Integer, Integer> m = new HashMap<>();
			for (int i = 0; i < input.length; i++)
				m.put(i, input[i]);
			return new State(m, fuel);
		}
		
	}
	

	public static class Increment extends WhileProgram {
		
		public final int var1, var2, amount;

		public Increment(int var1, int var2, int amount) {
			super();
			if (amount < 0)
				throw new IllegalArgumentException("Increment amount must be non-negative.");
			if (var1 < 0 || var2 < 0)
				throw new IllegalArgumentException("Variable indices must be non-negative.");
			this.var1 = var1;
			this.var2 = var2;
			this.amount = amount;
		}

		@Override
		public void execute(State st) throws OutOfFuelException {
			st.tick();
			st.set(var1, st.get(var2) + amount);
		}

		@Override
		protected void addVariables(Set<Integer> s) {
			s.add(var1);
			s.add(var2);
		}

		public <A> A accept(Visitor<A> v) {
			return v.visitIncrement(var1, var2, amount);
		}

	}
	
	public static class Decrement extends WhileProgram {
		
		public final int var1, var2, amount;

		public Decrement(int var1, int var2, int amount) {
			super();
			if (amount < 0)
				throw new IllegalArgumentException("Increment amount must be non-negative.");
			if (var1 < 0 || var2 < 0)
				throw new IllegalArgumentException("Variable indices must be non-negative.");
			this.var1 = var1;
			this.var2 = var2;
			this.amount = amount;
		}
		
		@Override
		public void execute(State st) throws OutOfFuelException {
			st.tick();
			st.set(var1, Math.max(0,  st.get(var2) - amount));
		}
		
		@Override
		protected void addVariables(Set<Integer> s) {
			s.add(var1);
			s.add(var2);
		}
		
		public <A> A accept(Visitor<A> v) {
			return v.visitDecrement(var1, var2, amount);
		}
		
	}
	
	public static class Block extends WhileProgram {
		
		public final List<WhileProgram> parts;

		public Block(List<WhileProgram> parts) {
			super();
			Objects.requireNonNull(parts);
			for (WhileProgram p : parts)
				Objects.requireNonNull(p);
			this.parts = Collections.unmodifiableList(parts);
		}

		@Override
		public void execute(State st) throws OutOfFuelException {
			st.tick();
			for (WhileProgram part : parts)
				part.execute(st);
		}
		

		@Override
		protected void addVariables(Set<Integer> s) {
			for (WhileProgram part : parts)
				part.addVariables(s);
		}

		public <A> A accept(Visitor<A> v) {
			return v.visitBlock(parts);
		}
		
	}
	
	public static class IfThenElse extends WhileProgram {
		
		public final int testVar;
		public final WhileProgram thenBody, elseBody;
		
		public IfThenElse(int testVar, WhileProgram thenBody, WhileProgram elseBody) {
			super();
			if (testVar < 0)
				throw new IllegalArgumentException("Variable indices must be non-negative.");
			this.testVar = testVar;
			this.thenBody = Objects.requireNonNull(thenBody);
			this.elseBody = Objects.requireNonNull(elseBody);
		}

		@Override
		public void execute(State st) throws OutOfFuelException {
			st.tick();
			if (st.get(testVar) == 0)
				thenBody.execute(st);
			else
				elseBody.execute(st);
		}
		

		@Override
		protected void addVariables(Set<Integer> s) {
			s.add(testVar);
			thenBody.addVariables(s);
			elseBody.addVariables(s);
		}

		public <A> A accept(Visitor<A> v) {
			return v.visitIfThenElse(testVar, thenBody, elseBody);
		}
		
	}
	
	public static class While extends WhileProgram {
		
		public final int testVar;
		public final WhileProgram loopBody;
		
		public While(int testVar, WhileProgram loopBody) {
			super();
			if (testVar < 0)
				throw new IllegalArgumentException("Variable indices must be non-negative.");
			this.testVar = testVar;
			this.loopBody = Objects.requireNonNull(loopBody);
		}

		@Override
		public void execute(State st) throws OutOfFuelException {
			st.tick();
			while (st.get(testVar) != 0) {
				loopBody.execute(st);
				st.tick();
			}
		}

		@Override
		protected void addVariables(Set<Integer> s) {
			s.add(testVar);
			loopBody.addVariables(s);
		}
		
		public <A> A accept(Visitor<A> v) {
			return v.visitWhile(testVar, loopBody);
		}
		
	}
	
	public abstract void execute(State st) throws OutOfFuelException;
	
	protected abstract void addVariables(Set<Integer> s);
	
	public abstract <A> A accept(Visitor<A> v);
	
	public Set<Integer> getVariables() {
		Set<Integer> s = new HashSet<>();
		addVariables(s);
		return s;
	}
	
	public int getVariableCount() {
		return getVariables().stream().max(Integer::compare).map(i -> i + 1).orElse(0);
	}
	
	public static WhileProgram parse(String s) {
		return (new WhileProgramParser(s)).parse();
	}
	
	public TuringMachine toTuringMachine() {
		return WhileToTM.run(this);
	}
	
	public String toString() {
		ToStringVisitor v = new ToStringVisitor();
		accept(v);
		return v.sb.toString();
	}
	
	private class ToStringVisitor implements Visitor<Void> {
		
		String currentIndent = "";
		StringBuilder sb = new StringBuilder();
		
		private void increaseIndent() {
			currentIndent = currentIndent + "  ";
		}
		
		private void decreaseIndent() {
			currentIndent = currentIndent.substring(2);
		}

		@Override
		public Void visitIncrement(int i, int j, int amount) {
			sb.append(String.format("x%d := x%d + %d", i, j, amount));
			return null;
		}

		@Override
		public Void visitDecrement(int i, int j, int amount) {
			sb.append(String.format("x%d := x%d - %d", i, j, amount));
			return null;
		}

		@Override
		public Void visitBlock(List<WhileProgram> parts) {
			if (parts.isEmpty())
				return null;
			Iterator<WhileProgram> it = parts.iterator();
			it.next().accept(this);
			while (it.hasNext()) {
				sb.append(";\n").append(currentIndent);
				it.next().accept(this);
			}
			return null;
		}

		@Override
		public Void visitIfThenElse(int testVar, WhileProgram thenBody, WhileProgram elseBody) {
			sb.append(String.format("IF x%d = 0 THEN\n", testVar));
			increaseIndent();
			sb.append(currentIndent);
			thenBody.accept(this);
			decreaseIndent();
			sb.append("\n").append(currentIndent).append("ELSE\n");
			increaseIndent();
			sb.append(currentIndent);
			elseBody.accept(this);
			decreaseIndent();
			sb.append("\n").append(currentIndent).append("END");
			return null;
		}

		@Override
		public Void visitWhile(int testVar, WhileProgram loopBody) {
			sb.append(String.format("WHILE x%d != 0 DO\n", testVar));
			increaseIndent();
			sb.append(currentIndent);
			loopBody.accept(this);
			decreaseIndent();
			sb.append("\n").append(currentIndent).append("END");
			return null;
		}
		
	}
	
	
}
