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

    @abstractmethod
    def is_empty(self):
        pass

    @abstractmethod
    def is_trivial(self):
        pass

    @abstractmethod
    def is_finite(self):
        pass

    @abstractmethod
    def get_language(self):
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

    def is_empty(self):
        return False

    def is_trivial(self):
        return False

    def is_finite(self):
        return True

    def get_language(self):
        return set(self.c)

    def to_epsilon_nfa(self, alphabet):
        start = State()
        end = State()
        t = Transition(start, end, self.c)

        return Epsilon_NFA(set([start, end]), set([t]), alphabet, start, set([t.end]))


class Empty(Regex):

    def __str__(self):
        return "∅"

    def get_precedence(self):
        return 4

    def is_empty(self):
        return True

    def is_trivial(self):
        return True

    def is_finite(self):
        return True

    def get_language(self):
        return set()

    def to_epsilon_nfa(self, alphabet):
        start = State()
        return Epsilon_NFA(set([start]), set(), alphabet, start, set())


class Epsilon(Regex):

    def __str__(self):
        return "ε"

    def get_precedence(self):
        return 4

    def is_empty(self):
        return False

    def is_trivial(self):
        return True

    def is_finite(self):
        return True

    def get_language(self):
        return set([""])

    def to_epsilon_nfa(self, alphabet):
        start = State()
        return Epsilon_NFA(set([start]), set(), alphabet, start, set([start]))


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

    def is_empty(self):
        return self.r1.is_empty() and self.r2.is_empty()

    def is_trivial(self):
        return self.r1.is_trivial() and self.r2.is_trivial()

    def is_finite(self):
        return self.r1.is_finite() and self.r2.is_finite()

    def get_language(self):
        l1 = self.r1.get_language()
        l2 = self.r2.get_language()
        if l1 != None and l2 != None:
            return l1 | l2
        return None

    def to_epsilon_nfa(self, alphabet):
        n1 = self.r1.to_epsilon_nfa(alphabet)
        n2 = self.r2.to_epsilon_nfa(alphabet)
        start = State()
        transitions = n1.transitions | n2.transitions
        transitions.add(Transition(start, n1.start_state, None))
        transitions.add(Transition(start, n2.start_state, None))
        states = n1.states | n2.states
        states.add(start)
        return Epsilon_NFA(states, transitions, alphabet, start, n1.final_states | n2.final_states)


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

    def is_empty(self):
        return self.r1.is_empty() or self.r2.is_empty()

    def is_trivial(self):
        return self.r1.is_empty() or self.r2.is_empty() or self.r1.is_trivial() and self.r2.is_trivial()

    def is_finite(self):
        return self.r1.is_empty() or self.r2.is_empty() or self.r1.is_finite() and self.r2.is_finite()

    def get_language(self):
        l1 = self.r1.get_language()
        l2 = self.r2.get_language()
        if l1 == set() or l2 == set():
            return set()
        elif l1 != None and l2 != None:
            return {x + y for x in l1 for y in l2}
        return None

    def to_epsilon_nfa(self, alphabet):
        n1 = self.r1.to_epsilon_nfa(alphabet)
        n2 = self.r2.to_epsilon_nfa(alphabet)
        transitions = n1.transitions | n2.transitions
        transitions |= set(Transition(s, n2.start_state, None)
                           for s in n1.final_states)
        return Epsilon_NFA(n1.states | n2.states, transitions, alphabet, n1.start_state, n2.final_states)


class Star(Regex):
    def __init__(self, r):
        self.r = r

    def __str__(self):
        s = self.r.to_str(self.get_precedence())
        return "{}*".format(s)

    def get_precedence(self):
        return 2

    def is_empty(self):
        return False

    def is_trivial(self):
        return self.r.is_trivial()

    def is_finite(self):
        return self.r.is_trivial()

    def get_language(self):
        l = self.r.get_language()
        if l == set() or l == set([""]):
            return set([""])
        return None

    def to_epsilon_nfa(self, alphabet):
        n = self.r.to_epsilon_nfa(alphabet)
        ts = n.transitions
        ts |= set(Transition(s, n.start_state, None) for s in n.final_states)
        start = State()
        ts.add(Transition(start, n.start_state, None))
        return Epsilon_NFA(n.states | set([start]), ts, alphabet, start, n.final_states | set([start]))


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
