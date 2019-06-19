from context_free_grammar import Production, Context_Free_Grammar


class PDA_Transition:
	EPSILON = '\u03B5'

	def __init__(self, start, label, pop_symbol, end, push_symbols):
		self.start = start
		self.label = label
		self.pop_symbol = pop_symbol
		self.end = end
		self.push_symbols = push_symbols

	def __str__(self):
		return "{};{};{};{};{}".format(self.start, "" if self.label == EPSILON else self.label, self.pop_symbol, self.end, self.push_symbols)

	def __hash__(self):
		return hash((self.start, self.end, self.label, self.pop_symbol, self.push_symbols))

	def __eq__(self, other):
		if not isinstance(other, PDA_Transition):
			return False
		return (self.start, self.end, self.label, self.pop_symbol, self.push_symbols) == (other.start, other.end, other.label, other.pop_symbol, other.push_symbols)


class PDA:
	def __init__(self, states, transitions, alphabet, start_state, stack_alphabet, start_symbol):
		self.states = states
		self.transitions = transitions
		self.alphabet = alphabet
		self.start_state = start_state
		self.stack_alphabet = stack_alphabet
		self.start_symbol = start_symbol

	def parse(lines):
		lines = lines[::-1]

		def strip_or_raise(line, to_strip, err_msg):
			line = line.strip()
			if not line.startswith(to_strip):
				raise ValueError(err_msg)
			return line[len(to_strip):].strip()

		# First line
		if not lines.pop().strip() == "PDA":
			raise ValueError("Parsed automaton does not start with PDA.")

		# Second line
		alphabet = strip_or_raise(
			lines.pop(), "Alphabet:", "Parsed automaton does not declare alphabet first.")
		alphabet = set(alphabet.split(";"))
		for letter in alphabet:
			if len(letter) != 1:
				raise ValueError(
					"Letters have to be input as a semicolon separated list without spaces. Letters may only be chars.")

		# Third line; states
		# Second line
		states = strip_or_raise(
			lines.pop(), "States:", "Parsed automaton does not declare states second.")
		states = set(states.split(";"))

		# Fourth line; initial state
		init_state = strip_or_raise(
			lines.pop(), "Init:", "Parsed automaton does not declare initial state third.")

		# Fifth line; stack alphabet
		stack_alphabet = strip_or_raise(
			lines.pop(), "Stackalphabet:", "Parsed automaton does not declare stackalphabet fourth.")
		stack_alphabet = set(stack_alphabet.split(";"))
		for letter in stack_alphabet:
			if len(letter) != 1:
				raise ValueError(
					"Stack symbols have to be input as a semicolon separated list without spaces. Letters may only be chars.")

		# Sixth line; start symbol
		start_symbol = strip_or_raise(
			lines.pop(), "Startsymbol:", "Parsed automaton does not declare stackalphabet fourth.")
		if len(start_symbol) != 1:
			raise ValueError("Startsymbol must be a char, but has length != 1")

		# Seventh line; transitions
		transitions = strip_or_raise(
			lines.pop(), "Transitions:", "Parsed automaton does not declare transitions sixth.")
		transitions = set()
		transition = lines.pop().strip()
		while transition != "END":
			split = transition.split(";")
			if len(split) < 4 or len(split) > 5:
				raise ValueError("Invalid transition {}".format(transition))
			start, letter, pop_symbol, end, push_symbols = split
			if len(letter) > 1:
				raise ValueError(
					"Transition label may only be a char, but it is " + letter)
			label = PDA_Transition.EPSILON if len(letter) == 0 else letter

			if not start in states or not end in states:
				raise ValueError(
					"The states for a transition are not in the state set ({} or {})".format(start, end))

			if len(pop_symbol) > 1:
				raise ValueError(
					"Popped symbol may only be a char, but it is " + pop_symbol)
			if len(pop_symbol) == 0:
				raise ValueError(
					"Popped symbol must be char, but it is empty word")
			transitions.add(PDA_Transition(
				start, label, pop_symbol, end, push_symbols))
			transition = lines.pop().strip()

		if init_state not in states:
			raise ValueError("Initial state is not in state set")

		return PDA(states, transitions, alphabet, init_state, stack_alphabet, start_symbol)

	# Return a CFG that is equivalent to this automaton(cf. Theorem 4.60 from the lecture).
	# TODO
	def to_cfg(self):
		# Create a triple-non-terminal.
		def triple(q1, z, q2):
			return "[{};{};{}]".format(q1, z, q2)

		return None
