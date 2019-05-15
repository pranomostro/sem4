from abc import ABC, abstractmethod
import sys
from epsilon_nfa import State, Transition, Epsilon_NFA


class Regex(ABC):

    def __init__(self):
        pass

    def to_str(self, precedence):
        if precedence > self.get_precedence():
            return "({})".format(str(self))
        return str(self)

    @abstractmethod
    def get_precedence(self):
        pass

    @abstractmethod
    def __str__(self):
        pass

    # Should return an instance of Epsilon_NFA
    @abstractmethod
    def to_epsilon_nfa(self, alphabet):
        pass


class Single(Regex):

    def __init__(self, c):
        if len(c) != 1:
            raise ValueError("Character should be a string of length 1.")
        self.c = c

    def __str__(self):
        return self.c

    def get_precedence(self):
        return 4

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


class Empty(Regex):

    def __str__(self):
        return "∅"

    def get_precedence(self):
        return 4

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


class Epsilon(Regex):

    def __str__(self):
        return "ε"

    def get_precedence(self):
        return 4

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


class Alternative(Regex):

    def __init__(self, r1, r2):
        self.r1 = r1
        self.r2 = r2

    def __str__(self):
        s1 = self.r1.to_str(self.get_precedence())
        s2 = self.r2.to_str(self.get_precedence())
        return "{} | {}".format(s1, s2)

    def get_precedence(self):
        return 0

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


class Concat(Regex):

    def __init__(self, r1, r2):
        self.r1 = r1
        self.r2 = r2

    def __str__(self):
        s1 = self.r1.to_str(self.get_precedence())
        s2 = self.r2.to_str(self.get_precedence())
        return "{}{}".format(s1, s2)

    def get_precedence(self):
        return 3

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


class Star(Regex):
    def __init__(self, r):
        self.r = r

    def __str__(self):
        s = self.r.to_str(self.get_precedence())
        return "{}*".format(s)

    def get_precedence(self):
        return 2

    # TODO
    def to_epsilon_nfa(self, alphabet):
        pass


# Parser -- can be ignored

def is_symbol(c: str) -> str:
    return c.isalnum()


class Regex_Parser:

    def __init__(self, s: str):
        self.input = s
        self.pos = 0

    def eof(self):
        return self.pos >= len(self.input)

    def peek(self):
        while not self.eof() and self.input[self.pos].isspace():
            self.pos += 1
        if self.eof():
            return None
        return self.input[self.pos]

    def pretty_peek(self) -> str:
        c = self.peek()
        if c:
            return "'{}'".format(c)
        return "end of input"

    def parse(self) -> Regex:
        r = self.parse_alternative()
        if not self.eof():
            raise ValueError(
                "Expected end of input, but got {} instead at position {}.".format(self.pretty_peek(), self.pos))

        return r

    def parse_alternative(self):
        r = self.parse_non_alternative()
        c = self.peek()
        if c == None or c == ')':
            return r
        if c == '|':
            self.pos += 1
            r2 = self.parse_alternative()
            return Alternative(r, r2)

        raise ValueError(
            "Expected '|' or end of input, but got {} instead at position {}.".format(self.pretty_peek(), self.pos))

    def parse_non_alternative(self):
        c = self.peek()

        if c == None or c == ')' or c == '|':
            return Epsilon()

        r = None
        while True:
            r2 = None
            if c == '∅':
                r2 = Empty()
            elif c == 'ε':
                r2 = Epsilon()
            elif c == '(':
                self.pos += 1
                r2 = self.parse_alternative()
                if (self.peek() != ')'):
                    raise ValueError("Expected ')', but got {} instead at position {}.".format(
                        self.pretty_peek(), self.pos))
            elif is_symbol(c):
                r2 = Single(c)
            elif c == '*':
                r2 = Star(Epsilon())
            else:
                raise ValueError(
                    "Expected alphabet symbol or one of '(', '|', ')', '*', but got {} instead at position {}.".format(self.pretty_peek(), self.pos))

            self.pos += 1

            while(self.peek() == '*'):
                r2 = Star(r2)
                self.pos += 1

            if r == None:
                r = r2
            else:
                r = Concat(r, r2)

            c = self.peek()

            if c == None or c == ')' or c == '|':
                break

        return r
