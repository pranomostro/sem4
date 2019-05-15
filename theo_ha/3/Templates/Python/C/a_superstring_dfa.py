from epsilon_nfa import State, Transition, DFA
import sys

# Returns the longest prefix of haystack that is a suffix of needle.
# This can be handy but you do not /need/ to implement it.

# TODO
def get_longest_suffix(haystack, needle):
    pass


# Given a string s, return the minimal DFA that accepts
# exactly all superstrings of s. The alphabet is implicitly given
# by the characters in string s.

# TODO
def superstring_dfa(s):
    pass


# Main function -- can be ignored.
if __name__ == "__main__":
    print(superstring_dfa(sys.stdin.readline().strip()))
