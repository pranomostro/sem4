import sys

# Eine Produktion parsen.
# Produtkionen werden einfach als Paare der Form (linke Seite, rechte Seite) dargestellt.
def parse_production(line):
    l, r = line.split('->')
    l = l.strip()
    r = r.strip()
    return l, r

# Hilfsfunktionen
def is_terminal(s):
    return s.islower()

def is_non_terminal(s):
    return s.isupper()

def is_start_symbol(s):
    return s == "S"

# TODO
def get_type(prod):
    lhs, rhs = prod
    r = 0

    if len(lhs) <= len(rhs):
        r = 1

    if r >= 1 and len(lhs) == 1 and is_non_terminal(lhs[0]):
        r = 2

    if len(lhs) == 1 and is_start_symbol(lhs[0]) and rhs == "":
        r = 3
    
    if r >= 2 and len(rhs) == 1 and is_terminal(rhs[0]):
        r = 3

    if r >= 2 and len(rhs) == 2 and is_terminal(rhs[0]) and is_non_terminal(rhs[1]):
        r = 3

    return r

if __name__ == "__main__":
    for line in sys.stdin:
        if line.strip() != "END":
            prod = parse_production(line)
            print(get_type(prod))