def apply(w, lhs, rhs):
	sw_lhs=w.split(lhs)
	if len(sw_lhs)<=1:
		return w
	return apply_rec(sw_lhs[1:], sw_lhs[0], lhs, rhs)

def apply_rec(w, res, lhs, rhs):
	# Only pass arguments where len(w) initially is >=2
	if len(w)==0:
		return res
	return apply_rec(w[1:], [i+rhs+w[0] for i in res]+[j+lhs+w[0] for j in res], lhs, rhs)
