from epsilon_nfa import State, Transition, DFA
import sys


def get_longest_suffix(haystack, needle):
    for i in range(len(needle) + 1):
        if haystack[:len(needle) - i] == needle[i:]:
            return needle[i:]


def superstring_dfa(s):
    alphabet = set(s)
    states = dict((s[:i], State("_" + s[:i])) for i in range(0, len(s) + 1))
    transitions = set(Transition(states[s[:i]], states[get_longest_suffix(
        s, s[:i] + a)], a) for a in alphabet for i in range(len(s)))
    transitions |= set(Transition(states[s], states[s], a) for a in alphabet)
    return DFA(set(states.values()), transitions, alphabet, states[""], set([states[s]]))


if __name__ == "__main__":
    print(superstring_dfa(sys.stdin.readline().strip()))
