from abc import ABC, abstractmethod
import sys
from epsilon_nfa import State, Transition, Epsilon_NFA


class Regex(ABC):

    single_instances = {}

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
    def is_nullable(self):
        pass

    # Smart constructors
    @staticmethod
    def empty():
        return Empty()

    @staticmethod
    def epsilon():
        return Epsilon()

    @staticmethod
    def single(c):
        if c in Regex.single_instances:
            return Regex.single_instances[c]
        r = Single(c)
        Regex.single_instances[c] = r
        return r

    @staticmethod
    def alternatives(regexes, *args):
        if len(args) > 0:
            regexes = [regexes] + list(args)
        s = set()
        nullable = False
        has_epsilon = False

        for r in regexes:
            if isinstance(r, Epsilon):
                has_epsilon = True
            else:
                nullable = nullable or r.is_nullable()
                if isinstance(r, Alternative):
                    for r2 in r:
                        s.add(r)
                elif not isinstance(r, Empty):
                    s.add(r)

        if has_epsilon and not nullable:
            s.add(Regex.epsilon())

        if len(s) == 0:
            return Regex.empty()
        elif len(s) == 1:
            return s.pop()
        else:
            return Alternative(s)

    @staticmethod
    def concats(regexes, *args):
        if len(args) > 0:
            regexes = [regexes] + list(args)
        l = []
        for r in regexes:
            if isinstance(r, Empty):
                return Regex.empty()
            elif isinstance(r, Concat):
                for r2 in r:
                    l.append(r2)
            elif not isinstance(r, Epsilon):
                l.append(r)

        if len(l) == 0:
            return Regex.epsilon()
        elif len(l) == 1:
            return l[0]
        else:
            return Concat(l)

    @staticmethod
    def star(r):
        if isinstance(r, Star):
            return r
        elif isinstance(r, Empty) or isinstance(r, Epsilon):
            return Regex.epsilon()
        else:
            return Star(r)


class Single(Regex):

    def __init__(self, c):
        if len(c) != 1:
            raise ValueError("Character should be a string of length 1.")
        self.c = c
        self.size = 1

    def __str__(self):
        return self.c

    def __hash__(self):
        prime = 31
        return prime * 1 + ord(self.c)

    def __eq__(self, other):
        if not isinstance(other, Single):
            return False
        return self.c == other.c

    def get_precedence(self):
        return 4

    def is_nullable(self):
        return False

    def to_epsilon_nfa(self, alphabet):
        start = State()
        end = State()
        t = Transition(start, end, self.c)

        return Epsilon_NFA(set([start, end]), set([t]), alphabet, start, set([t.end]))


class Empty(Regex):

    def __init__(self):
        self.size = 1

    def __str__(self):
        return "{}"

    def __hash__(self):
        return 23

    def __self__(self, other):
        if not isinstance(other, Empty):
            return False
        return True

    def get_precedence(self):
        return 4

    def is_nullable(self):
        return False

    def get_language(self):
        return set()

    def to_epsilon_nfa(self, alphabet):
        start = State()
        return Epsilon_NFA(set([start]), set(), alphabet, start, set())


class Epsilon(Regex):

    def __init__(self):
        self.size = 1

    def __str__(self):
        return "()"

    def __hash__(self):
        return 47

    def __self__(self, other):
        if not isinstance(other, Epsilon):
            return False
        return True

    def get_precedence(self):
        return 4

    def is_nullable(self):
        return True

    def to_epsilon_nfa(self, alphabet):
        start = State()
        return Epsilon_NFA(set([start]), set(), alphabet, start, set([start]))


class Alternative(Regex):

    def __init__(self, regexes):
        children = set()
        size = 1
        nullable = False
        for r in regexes:
            if isinstance(r, Empty):
                continue
            children.add(r)
            size += r.size
            nullable = nullable or r.is_nullable()
        self.children = children
        self.size = size
        self.nullable = nullable

    def __str__(self):
        return "|".join(r.to_str(self.get_precedence()) for r in self.children)

    def __iter__(self):
        return iter(self.children)

    def __eq__(self, other):
        if not isinstance(other, Alternative):
            return False
        return other.children == self.children

    def __hash__(self):
        prime = 31
        result = 1
        result = result * prime + \
            (hash(tuple(self.children)) if self.children else 0)
        result = result * prime + self.size
        return result

    def get_precedence(self):
        return 0

    def is_nullable(self):
        return self.nullable

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

    def __init__(self, regexes):
        children = list()
        size = 1
        nullable = True
        for r in regexes:
            if not isinstance(r, Epsilon):
                children.append(r)
                size += r.size
                nullable = nullable and r.is_nullable()
        self.size = size
        self.nullable = nullable
        self.children = children

    def __str__(self):
        return "".join(r.to_str(self.get_precedence()) for r in self.children)

    def __iter__(self):
        return iter(self.children)

    def __eq__(self, other):
        if not isinstance(other, Concat):
            return False
        return other.children == self.children

    def __hash__(self):
        prime = 31
        result = 1
        result = result * prime + \
            (hash(tuple(self.children)) if self.children else 0)
        result = result * prime + self.size
        return result

    def get_precedence(self):
        return 3

    def is_nullable(self):
        return self.nullable

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
        self.size = r.size + 1

    def __str__(self):
        s = self.r.to_str(self.get_precedence())
        return "{}*".format(s)

    def __eq__(self, other):
        if not isinstance(other, Star):
            return False
        return other.r == self.r

    def __hash__(self):
        prime = 31
        result = 1
        result = result * prime + (hash(self.r) if self.r else 0)
        result = result * prime + self.size
        return result

    def get_precedence(self):
        return 2

    def is_nullable(self):
        return True

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
        l = [r]
        c = self.peek()
        while c == '|':
            self.pos += 1
            l.append(self.parse_alternative())
            c = self.peek()

        if c != None and c != ')':
            raise ValueError(
                "Expected '|' or end of input, but got {} instead at position {}.".format(self.pretty_peek(), self.pos))

        return Regex.alternatives(l)

    def parse_non_alternative(self):
        c = self.peek()

        if c == None or c == ')' or c == '|':
            return Regex.epsilon()

        l = []
        r = None
        while True:
            if c == '{':
                self.pos += 1
                r = Regex.empty()
                if self.peek() != '}':
                    raise ValueError(
                        "Expected '}', but got {} instead at position {}.".format(self.pretty_peek(), self.pos))
            elif c == '∅':
                r = Regex.empty()
            elif c == 'ε' or c == '*':
                r = Regex.epsilon()
            elif c == '(':
                self.pos += 1
                r = self.parse_alternative()
                if (self.peek() != ')'):
                    raise ValueError("Expected ')', but got {} instead at position {}.".format(
                        self.pretty_peek(), self.pos))
            elif is_symbol(c):
                r = Regex.single(c)
            else:
                raise ValueError(
                    "Expected alphabet symbol or one of '(', '|', ')', '*', but got {} instead at position {}.".format(self.pretty_peek(), self.pos))

            self.pos += 1

            while(self.peek() == '*'):
                r = Regex.star(r)
                self.pos += 1

            l.append(r)

            c = self.peek()

            if c == None or c == ')' or c == '|':
                break

        return Regex.concats(l)


epsilon = Regex.epsilon
empty = Regex.empty
single = Regex.single
star = Regex.star
concats = Regex.concats
alternatives = Regex.alternatives
