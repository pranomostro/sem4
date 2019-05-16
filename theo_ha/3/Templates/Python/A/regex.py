from abc import ABC, abstractmethod
import sys
import io


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

	# We recommend to compute the language of the regex as Python set.
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

	def get_language(self):
		return [self.c]


class Empty(Regex):

	def __str__(self):
		return "∅"

	def get_precedence(self):
		return 4

	def is_empty(self):
		return True

	def is_trivial(self):
		return True

	def get_language(self):
		return []


class Epsilon(Regex):

	def __str__(self):
		return "ε"

	def get_precedence(self):
		return 4

	def is_empty(self):
		return False

	def is_trivial(self):
		return True

	def get_language(self):
		return ["ε"]


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

	# TODO
	def get_language(self):
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

	def is_empty(self):
		return self.r1.is_empty() or self.r2.is_empty()

	def is_trivial(self):
		return self.is_empty() or (self.r1.is_trivial() and self.r2.is_trivial())

	# TODO
	def get_language(self):
		pass


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

	def get_language(self):
		return None


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


# Main function -- can be ignored
if __name__ == "__main__":
	input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')

	mode = None

	for line in input_stream.readlines():
		line = line.strip()
		if not mode:
			if line == "Trivial":
				mode = "trivial"
				continue
			elif line == "Language":
				mode = "language"
				continue
			else:
				print("Invalid mode!")
				sys.exit(1)

		if line == "END":
			break

		rp = Regex_Parser(line)
		r = rp.parse()

		if mode == "trivial":
			print("yes" if r.is_trivial() else "no")
		else:
			l = r.get_language()
			if l == None:
				sys.stdout.buffer.write("infinite\n".encode('utf-8'))
			else:
				l = ["ε" if len(w) == 0 else w for w in sorted(l)]
				s = "{%s}\n" % ", ".join(l)
				sys.stdout.buffer.write(s.encode('utf-8'))
