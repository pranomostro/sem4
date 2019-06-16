import sys
import io


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

    # Returns the set of all productions whose left-hand side consists of the given symbol.
    def get_productions_by_lhs(self, lhs):
        if lhs in self.production_map:
            return self.production_map[lhs]
        else:
            return set()

    # Returns the set of all productions with the given right-hand side.
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

    # TODO: Fill in this part of the code.
    # Should return the set of productive non-terminals.
    def get_productive_non_terminals(self):
        return set()


def do_productive():
    input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    lines = input_stream.readlines()
    g = Context_Free_Grammar.parse(lines)
    for non_terminal in sorted(g.get_productive_non_terminals()):
        sys.stdout.buffer.write((str(non_terminal) + "\n").encode('utf-8'))


if __name__ == "__main__":
    do_productive()
