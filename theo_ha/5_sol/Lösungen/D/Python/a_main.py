# We strongly advise against changing the following line
from regex import Regex_Parser, Visitor, epsilon, empty, single, star, alternatives, concats
import sys
import io


class Regex_Remainder(Visitor):
    def __init__(self, c):
        self.c = c

    def visit_empty(self):
        return empty()

    def visit_epsilon(self):
        return empty()

    def visit_single(self, c):
        if self.c == c:
            return epsilon()
        return empty()

    def visit_alternative(self, rs):
        return alternatives(r.accept(self) for r in rs)

    def visit_concat(self, rs):
        result = [concats(rs[:-1] + [rs[-1].accept(self)])]
        for i in range(len(rs) - 1, 0, -1):
            if not rs[i].is_nullable():
                break
            result.append(concats(rs[:i - 1] + [rs[i - 1].accept(self)]))
        return alternatives(result)

    def visit_star(self, r):
        return concats(star(r), r.accept(self))


def f(r, a):
    return r.accept(Regex_Remainder(a))


def g(r, w):
    for a in w[::-1]:
        r = f(r, a)
    return r

def regex_matches(r, w):
    return g(r, w).is_nullable()


if __name__ == "__main__":
    input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    lines = input_stream.readlines()
    if len(lines) < 2:
        print("Invalid input: expecting more lines!")
        sys.exit(1)
    for line in lines:
        line = line.strip()
        if line == "END":
            break
        ws = line.split(";")
        r = Regex_Parser(ws[0]).parse()
        ws = ws[1:]
        results = ["{}: {}".format(
            w, "y" if regex_matches(r, w) else "n") for w in ws]
        print("; ".join(results))
