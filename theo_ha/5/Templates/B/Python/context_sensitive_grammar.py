from queue import Queue
import sys
import io


class Production:

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs
        for c in lhs + rhs:
            if not c.isalnum():
                raise ValueError(
                    "Illegal production:{} \u2192 {}".format(lhs, rhs))

    def __hash__(self):
        prime = 31
        result = 1
        result = prime * result + (0 if self.lhs == None else hash(self.lhs))
        result = prime * result + (0 if self.rhs == None else hash(self.rhs))
        return result

    def __eq__(self, other):
        if not isinstance(other, Production):
            return False
        return self.lhs == other.lhs and self.rhs == other.rhs

    def __str__(self):
        return self.lhs + " \u2192 " + self.rhs

    @staticmethod
    def parse(s):
        try:
            i = s.index('->')
        except ValueError:
            raise ValueError("Illegal production: '" + s + "'")
        lhs = s[0:i].strip()
        rhs = s[i+2:].strip()
        if lhs == "\u03b5":
            lhs = ""
        if rhs == "\u03b5":
            rhs = ""
        return Production(lhs, rhs)

    def apply(self, w):
        return None # TODO


class Context_Sensitive_Grammar:

    def __init__(self, start_symbol, productions):
        self.start_symbol = start_symbol
        self.productions = set(productions)
        epsilon_production = Production(start_symbol, "")
        contains_epsilon = epsilon_production in self.productions
        for p in productions:
            if len(p.rhs) < len(p.lhs) and not p == epsilon_production or contains_epsilon and start_symbol in p.rhs:
                raise ValueError("Non-context-sensitive production: " + str(p))

    def __iter__(self):
        return iter(productions)

    def __str__(self):
        lines = [
            "CSG",
            "Start symbol: " + self.start_symbol,
            "Productions"
        ]
        lines.extend(str(p) for p in self.productions)
        return "\n".join(lines)

    def parse(lines):
        def expect(s, colon):
            prefix = s + ":" if colon else s
            if not len(lines) > 0:
                raise ValueError(
                    "Expected '{}', but got end of input instead.".format(prefix))
            line = lines.pop().strip()
            if not line.startswith(prefix):
                raise ValueError(
                    "Expected '{}', but got '{}' instead.".format(prefix, line))
            if colon:
                return line[len(prefix):].strip()

        expect("CSG", False)
        start_symbol = expect("Start symbol", True)
        if len(start_symbol) != 1 or not start_symbol.isalnum():
            raise ValueError("Illegal start symbol " + start_symbol)

        expect("Productions", False)
        productions = set()
        while len(lines) > 0 and not lines[-1].strip() == "END":
            productions.add(Production.parse(lines.pop()))
        assert("END" == lines.pop().strip())
        return Context_Sensitive_Grammar(start_symbol, productions)

    def enumerate_words_raw(self, max_len):
        return None # TODO

    def enumerate_words(self, max_len):
        return None # TODO


if __name__ == "__main__":
    input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    lines = input_stream.readlines()
    lines.reverse()
    mode = lines.pop().strip()
    g = Context_Sensitive_Grammar.parse(lines)
    l = int(lines.pop())
    if mode == "Raw":
        words = g.enumerate_words_raw(l)
    elif mode == "Normal":
        words = g.enumerate_words(l)
    else:
        raise ValueError("Invalid mode!")
    words = sorted(words, key=lambda x: (len(x), x))
    for w in words:
        print('"{}"'.format(w))
    print("END")
