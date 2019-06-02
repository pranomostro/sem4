# We strongly advice against changing the following line
from regex import Regex_Parser, epsilon, empty, single, star, alternatives, concats
from epsilon_nfa import State, Transition, Epsilon_NFA
import sys
import io


class NFA_To_Regex:

	def __init__(self, nfa, full_mode=False):
		self.nfa = nfa
		self.full_mode = full_mode

	def get_labels_from_to(self, q1, q2):
		s = set()
		for t in nfa.transitions:
			if t.start == q1 and t.end == q2:
				s.add(t.label)
		return s

	def init_table(self):
		table={}
		for q1 in self.nfa.states:
			for q2 in self.nfa.states:
				labels=[single(i.label) for i in self.nfa.transitions if i.start==q1 and i.end==q2]
				if q1==q2:
					labels=labels+[epsilon()]
				if labels==[]:
					table[(q1, q2)]=empty()
				else:
					table[(q1, q2)]=alternatives(labels)
		return table

	def step(self, table, qk):
		newtable={}
		for q1 in self.nfa.states:
			for q2 in self.nfa.states:
				ov=table[(q1, q2)]
				rl=table[(q1, qk)]
				rrep=table[(qk, qk)]
				rr=table[(qk, q2)]
				newtable[(q1, q2)]=alternatives(ov, concats(rl, star(rrep), rr))
		return newtable

	def compute_result(self, table):
		fst=self.nfa.states[0]
		return alternatives([table[(fst, i)] for i in self.nfa.states])

	def run_computation(self):
		if self.full_mode:
			print("BEGIN NFA TO REGEX TRACE")
		i = 0
		table = self.init_table()
		self.print_table(i, None, table)
		for q in sorted(nfa.states):
			table = self.step(table, q)
			i += 1
			print()
			self.print_table(i, q, table)
		result = self.compute_result(table)
		self.print_result(result)
		if self.full_mode:
			print("END NFA TO REGEX TRACE")

	def print_table(self, i, q, table):
		if not self.full_mode:
			return
		print("Step " + str(i))
		if q != None:
			print("Processing state " + str(q))
		for q1 in nfa.states:
			for q2 in nfa.states:
				print("({}, {}): {}".format(q1, q2, table[(q1, q2)]))

	def print_result(self, r):
		if self.full_mode:
			print("\nFinal result: " + str(r))


def parse_nfa(lines):
	def parse_pair(key, f, s):
		k, v = s.split(":")
		v = v.strip()
		assert(k == key)
		return f(v)

	def filter_and_strip(xs):
		return [s.strip() for s in xs if len(s.strip()) > 0]

	assert(lines[0].strip() == "NFA")
	alphabet = parse_pair(
		"Alphabet", lambda x: filter_and_strip(x.split(";")), lines[1])
	states = parse_pair(
		"States", lambda x: filter_and_strip(x.split(";")), lines[2])
	init = parse_pair("Init", lambda x: x.strip(), lines[3])
	final = parse_pair(
		"Final", lambda x: filter_and_strip(x.split(";")), lines[4])
	parse_pair("Transitions", lambda x: x.strip(), lines[5])

	states = dict((s, State(s)) for s in states)
	init = states[init]
	final = list(states[s] for s in final)
	transitions = []
	for line in lines[6:]:
		if line.strip() == "END":
			break
		q, label, q1 = line.split(";")
		t = Transition(states[q.strip()], states[q1.strip()], label.strip())
		transitions.append(t)
	return Epsilon_NFA(list(states.values()), transitions, alphabet, init, final)


if __name__ == "__main__":
	input_stream = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
	lines = input_stream.readlines()
	if len(lines) < 2:
		print("Invalid input: expecting more lines!")
		sys.exit(1)
	mode = lines[0].strip()
	nfa = parse_nfa(lines[1:])
	comp = NFA_To_Regex(nfa, full_mode=(mode == "Full"))
	comp.run_computation()
