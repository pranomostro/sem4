import sys
import io

# Provides the table data structure needed for the CYK algorithm, i.e. a table with entries for any indices
# i,j with 1 ≤ i ≤ j ≤ n.


class CYK_Table:
	def __init__(self, size):
		if (size <= 0):
			raise ValueError("Invalid size")
		self.size = size
		self.table = {}
		for i in range(size):
			row = dict(enumerate(set() for _ in range(size - i)))
			self.table[i] = row

	# Can be ignored.
	def check_index(self, i, j):
		if i <= 0 or self.size < i:
			raise ValueError("Invalid index i {}".format(i))
		if j < i or self.size < j:
			raise ValueError("Invalid index j {}".format(j))

	# Can be ignored.
	def __str__(self):
		builder = []
		for row_index in range(self.size - 1, -1, -1):
			row = self.table[row_index]
			assert(row.length == self.size - row_index)
			for cell in row:
				builder.append("{{{}}}".format(",".join(map(str, cell))))
			if row_index > 0:
				builder.append("\n")
		return "".join(builder)

	# Retrieves the set of derivation trees stored for the given indices.
	def get(self, i, j):
		self.check_index(i, j)
		return self.table[j - i][i - 1]

	# Stores a set of derivation trees for the given indices.
	def set(self, i, j, *non_terminals):
		self.check_index(i, j)
		self.table[j - i][i - 1] = non_terminals

	# Adds a tree to the set of trees for the given indices.
	def add(self, i, j, non_terminal):
		self.check_index(i, j)
		self.table[j - i][i - 1].add(non_terminal)


class Production:
	def __init__(self, left, *right):
		self.left = left
		self.right = right
		self.check_valid()

	def check_valid(self):
		if not isinstance(self.left, str) or not all(isinstance(x, str) for x in self.right):
			raise ValueError(
				"Invalid production. All symbols of the production should be of type string: {} -> {}".format(self.left, self.right))

	def __str__(self):
		return "{} -> {}".format(self.left, " ".join(self.right))

	def __eq__(self, other):
		if not isinstance(other, Production):
			return False
		return self.left == other.left and self.right == other.right

	def __hash__(self):
		return hash((self.left, tuple(self.right)))

	def __lt__(self, other):
		return (self.left, self.right) < (other.left, other.right)


class Derivation_Tree:
	def __init__(self, grammar, symbol, *children):
		self.grammar = grammar
		self.symbol = symbol
		self.children = children
		self.check()

	def check(self):
		if not self.symbol in self.grammar.alphabet and not self.symbol in self.grammar.non_terminals:
			raise ValueError("Not a valid symbol: {}".format(self.symbol))
		if len(self.children) == 0:
			return
		if not self.symbol in self.grammar.non_terminals:
			raise ValueError(
				"Cannot make derivation of terminal: {}".format(self.symbol))
		rhs = [child.symbol for child in self.children]
		p = Production(self.symbol, *rhs)
		if not p in self.grammar.productions:
			raise ValueError("No such production:  {}".format(p))

	def __eq__(self, other):
		if not isinstance(other, Derivation_Tree):
			return False
		return self.symbol == other.symbol and self.children == other.children

	def __hash__(self):
		return hash((self.symbol, self.children))

	def build_string(self, string_builder, prefix, is_tail, is_first):
		if is_first:
			string_builder.append(self.symbol)
			string_builder.append("\n")
		else:
			string_builder.append("└" if is_tail else "├")
			string_builder.append(self.symbol)
			string_builder.append("\n")
			prefix = prefix + (" " if is_tail else "│")

		for child in self.children[:-1]:
			string_builder.append(prefix)
			child.build_string(string_builder, prefix, False, False)
		if len(self.children) > 0:
			string_builder.append(prefix)
			self.children[-1].build_string(string_builder, prefix, True, False)

		return string_builder

	def __str__(self):
		return "".join(self.build_string([], "", True, True))

	def __lt__(self, other):
		return (self.symbol, self.children) < (other.symbol, other.children)


class Context_Free_Grammar:
	def __init__(self, alphabet, non_terminals, productions, start_symbol):
		self.alphabet = alphabet
		self.non_terminals = non_terminals
		self.productions = productions
		self.start_symbol = start_symbol
		self.check_valid_grammar()

		self.production_map = dict()
		self.reverse_production_map = dict()
		for c in non_terminals:
			self.production_map[c] = set()
		for p in productions:
			self.production_map[p.left].add(p.right)
			s = set()
			if not p.right in self.reverse_production_map:
				self.reverse_production_map[p.right] = s
			else:
				s = self.reverse_production_map[p.right]
			s.add(p.left)

	def check_valid_grammar(self):
		if any(terminal in self.non_terminals for terminal in self.alphabet):
			raise ValueError("Terminals and non-terminals must be disjoint!")
		if not self.start_symbol in self.non_terminals:
			raise ValueError("Start symbol must be part of the non-terminals")
		atoms = set(self.non_terminals) | set(self.alphabet)
		for production in self.productions:
			if not production.left in atoms:
				raise ValueError(
					"{} is not in the grammar".format(production.left))
			for symbol in production.right:
				if not symbol in atoms:
					raise ValueError("{} is not in the grammar".format(symbol))

	@staticmethod
	def parse(lines):
		lines = lines[::-1]
		if lines.pop().strip() != "Grammar":
			raise ValueError("Parsed grammar does not start with 'Grammar'.")

		line = lines.pop()
		if not line.startswith("Nonterminals:"):
			raise ValueError(
				"Parsed grammar does not declare Nonterminals first.")
		non_terminals = set(line.strip("Nonterminals:").strip().split(","))

		line = lines.pop()
		if not line.startswith("Alphabet:"):
			raise ValueError(
				"Parsed grammar does not declare Alphabet second.")
		alphabet = set(line.strip("Alphabet:").strip().split(","))
		for alpha in alphabet:
			if len(alpha) != 1:
				raise ValueError(
					"Alphabet has to be input as a comma separated list without spaces. Terminals may only be chars.")

		line = lines.pop()
		if not line.startswith("Startsymbol:"):
			raise ValueError(
				"Parsed grammar does not declare start symbol third.")
		start_symbol = line.strip("Startsymbol:").strip()

		line = lines.pop()
		if not line.strip() == "Productions:":
			raise ValueError("Parsed grammar does not declare productions")

		productions = set()
		line = lines.pop().strip()
		while line != "END":
			if not '->' in line:
				raise ValueError(
					"Production {} does not contain '->'".format(line))
			left, rights = line.split('->')
			left = left.strip()
			if " " in left:
				raise ValueError(
					"Left-hand side must contain exactly one symbol.")
			rights = rights.strip().split("|")
			for right in rights:
				right = right.strip().split()
				right = (atom.strip()
						 for atom in right if not atom.isspace())
				productions.add(Production(left, *right))
			if len(lines) > 0:
				line = lines.pop().strip()
			else:
				raise ValueError("Input was not terminated with END")

		return Context_Free_Grammar(alphabet, non_terminals, productions, start_symbol)

	def get_productions_by_lhs(self, lhs):
		if lhs in self.production_map:
			return self.production_map[lhs]
		else:
			return set()

	def get_productions_by_rhs(self, *rhs):
		if rhs in self.reverse_production_map:
			return self.reverse_production_map[rhs]
		else:
			return set()

	def __str__(self):
		template = "\n".join([
			"Grammar",
			"Nonterminals: {}",
			"Alphabet: {}",
			"Startsymbol: {}",
			"Productions:{}",
			"END"
		])
		return template.format(
			",".join(sorted(self.non_terminals)),
			",".join(sorted(self.alphabet)), self.start_symbol,
			"\n" + "\n".join(sorted(map(str, self.productions))) if len(self.productions) > 0 else "")

	def put_prod(self, w, l, c, tab):
		if l==c:
			for p in self.productions:
				if w[l-1] in p.right:
					tab.add(l, c, Derivation_Tree(self, p.left, Derivation_Tree(self, w[l-1])))
		else:
			for k in range(l, c):
				for lft in tab.get(l, k):
					for rgt in tab.get(k+1, c):
						for p in self.productions:
							if (lft.symbol, rgt.symbol)==p.right:
								tab.add(l, c, Derivation_Tree(self, p.left, lft, rgt))

	# Computes the set of all derivation trees of the given word with respect to any non-terminal
	# of the grammar.
	def derivation_trees(self, w):
		der=CYK_Table(len(w))
		for i in range(0, len(w)):
			for j in range(i+1, len(w)+1):
				self.put_prod(w, j-i, j, der)
		return der.get(1, len(w))


def do_derivations():
	input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
	lines = input_stream.readlines()
	word = lines[-1].strip()
	g = Context_Free_Grammar.parse(lines)
	ts = sorted(g.derivation_trees(word))
	for t in ts:
		sys.stdout.buffer.write((str(t) + "\n").encode('utf-8'))

if __name__ == "__main__":
	do_derivations()
