# We strongly advise against changing the following line
from regex import Regex_Parser, Visitor, epsilon, empty, single, star, alternatives, concats
import sys
import io


class Regex_Remainder(Visitor):
    def __init__(self, c):
        self.c = c

    # TODO: Implement visitor methods
    def visit_empty(self):
        return None

    def visit_epsilon(self):
        return None

    def visit_single(self, c):
        return None

    def visit_alternative(self, rs):
        return None

    def visit_concat(self, rs):
        return None

    def visit_star(self, r):
        return None


def f(r, a):
    return r.accept(Regex_Remainder(a))


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
        c, r = line.split(";")
        r = Regex_Parser(r).parse()
        print(f(r, c))

