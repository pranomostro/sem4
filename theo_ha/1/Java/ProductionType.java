import java.util.Scanner;

public class ProductionType {

	public static class Production {
		final String left;
		final String right;

		/**
		 * Returns true iff the given symbol is a terminal.
		 */
		public static boolean isTerminal(char c) {
			return Character.isLowerCase(c);
		}

		/**
		 * Returns true iff the given symbol is a non-terminal.
		 */
		public static boolean isNonTerminal(char c) {
			return Character.isUpperCase(c);
		}

		/**
		 * Returns true iff the given symbol is the start symbol.
		 */
		public static boolean isStartSymbol(char c) {
			return c == 'S';
		}

		Production(String left, String right) {
			this.left = left;
			this.right = right;
		}

		@Override
		public String toString() {
			if (right.isEmpty()) {
				return left + " \u2192 " + "\u03B5"; // print the greek letter epsilon for empty right side
			} else {
				return left + " \u2192 " + right;
			}
		}

		/**
		 * Parses a production. Left and right side are separated by " -> ".
		 */
		public static Production parse(String s) {
			if (!s.contains("->")) {
				throw new IllegalArgumentException("Production '" + s + "' does not contain ' -> '");
			}
			String[] split = s.split("->");
			String left = split[0].trim();
			if (split.length == 1) { // "A -> "; empty production.
				return new Production(left, "");
			} else {
				return new Production(left, split[1].trim());
			}
		}

		/**
		 * Returns the type of a production as an integer between 0 and 3.
		 */
		public int getType() {
			return -1; // TODO
		}

	}

	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		String line;
		while (!(line = sc.nextLine()).equals("END")) {
			Production p = Production.parse(line);
			System.out.println(p.getType());
		}
		sc.close();
	}

}

