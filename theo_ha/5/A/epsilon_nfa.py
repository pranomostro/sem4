class State:
    id_counter = 0

    def __init__(self, name=""):
        self.id = State.id_counter
        State.id_counter += 1
        self.name = name

    def get_name(self):
        return self.name

    def __str__(self):
        if self.name != "":
            return self.name
        return "s" + str(self.id)

    def __hash__(self):
        return hash(self.id)

    def __lt__(self, other):
        return (self.name < other.name)

    @staticmethod
    def get_state_set(self, size):
        return set(State() for _ in range(size))


class Transition:

    def __init__(self, start, end, label):
        self.start = start
        self.end = end
        self.label = label

    def __str__(self):
        return "{};{};{}".format(self.start, 'ε' if self.label == None else self.label, self.end)

    def to_str_without_epsilon(self):
        return "{};{};{}".format(self.start, '' if self.label == None else self.label, self.end)

    def __hash__(self):
        return hash((self.start, self.end, self.label))

    def __lt__(self, other):
        return (str(self.start), self.label, str(self.end)) < (str(other.start), other.label, str(other.end))


class Epsilon_NFA:

    def __init__(self, states, transitions, alphabet, start_state, final_states):
        self.states = states
        self.transitions = transitions
        self.alphabet = alphabet
        self.start_state = start_state
        self.final_states = final_states
        self.check_valid_epsilon_nfa()

    def to_str(self):
        alphabet = ";".join(sorted(self.alphabet))
        states = ";".join(sorted(map(str, self.states)))
        final = ";".join(sorted(map(str, self.final_states)))
        transitions = "\n".join(t.to_str_without_epsilon()
                                for t in sorted(self.transitions))
        return "Alphabet: {}\nStates: {}\nInit: {}\nFinal: {}\nTransitions:\n{}\nEND".format(alphabet, states, self.start_state, final, transitions)

    def __str__(self):
        return "EpsilonNFA\n" + self.to_str()

    def check_valid_epsilon_nfa(self):
        pass


class DFA(Epsilon_NFA):

    def __str__(self):
        return "DFA\n" + self.to_str()
