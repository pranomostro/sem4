import sys

# Eine Produktion parsen.
# Produktionen werden einfach als Paare der Form (linke Seite, rechte Seite) dargestellt.
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

def get_type(prod):
	(f,s)=prod
	if not (is_start_symbol(f) and s=="") and len(f)>len(s):
		return 0
	if len(f)!=1:
		return 1
	if not (is_start_symbol(f) and s=="") and (s!="" and (s[0].isupper() or (s[1:]!="" and not s[1:].isupper()))):
		return 2
	return 3

if __name__ == "__main__":
	for line in sys.stdin:
		if line.strip() != "END":
			prod = parse_production(line)
			print(get_type(prod))
