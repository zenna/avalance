identity = ['fun', [['tv', 'a']], ['tv', 'a']]

inc_t = ['fun', 
	[['num']],
	['num']]

num = ['num']

#Q1. if tv, does which one is tv matter
#Q2. is bound  to a variable which containts tv then what
map_t = ['fun', 
	[['fun', [['tv', 'a']],
			 ['tv', 'b']],
	 ['list', ['tv', 'a']]], 
	['list', ['tv', 'a']]]

list_int_t = ['list', ['num']]

def is_func_matches_args(func_t, arg_ts):
	# Assume func_t is a func and arg lengths are correct
	func_args = func_t[1]

	# Get all bindings
	for i, arg_t in enumerate(func_args):
		same = is_same_type(arg_t, arg_ts[i], [True,[]])
		print same

	# Then check for consistency

# Saying whether two functions are the same type is different
# to saying whether a type matches a particular ag

# If they're both a primitive type, then return True
def combine_bindings(binding_1, binding_2):
	# If any one is false then the thing cant be type consistent can it?
	# import ipdb
	# ipdb.set_trace()
	type_consistent = binding_1[0] and binding_2[0]
	return [type_consistent, binding_1[1] + binding_2[1]]

# If its a list
# check first value
def is_same_type(t_1, t_2, tv_bindings):
	failed_binding = (False, [])
	t_1_typeclass = t_1[0]
	t_2_typeclass = t_2[0]

	# What about typevars?
	if (t_1_typeclass == "tv") != (t_2_typeclass == "tv"):
		if t_1_typeclass == "tv":
			tv_var = t_1
			other_var = t_2
		else:
			tv_var = t_2
			other_var = t_1

		# import ipdb
		# ipdb.set_trace()
		binding = [tv_var[1], other_var[0]]
		return combine_bindings(tv_bindings, [True, binding])

	elif (t_1_typeclass == "tv") and (t_2_typeclass == "tv"):
		print "both tv"
		return tv_binding

	elif t_1_typeclass != t_2_typeclass:
		print "type classes are different", t_1_typeclass, t_2_typeclass
		return combine_bindings(failed_binding, tv_bindings)

	elif t_1_typeclass == 'fun':
		t_1_args = t_1[1]
		t_2_args = t_2[1]

		# Must be the same number of arms
		if (len(t_1_args) != len(t_2_args)):
			print "arg_lengths are different"
			return combine_bindings(failed_binding, tv_bindings)
 
		new_bindings = tv_bindings
		for i in range(len(t_1_args)):
			new_bindings = is_same_type(t_1_args[i], t_2_args[i], new_bindings)

		t_1_ret = t_1[2]
		t_2_ret = t_2[2]
		new_bindings = is_same_type(t_1_ret, t_2_ret, new_bindings)
		return new_bindings

	elif (t_1[0] == 'list'):
		list_type_1 = t_1[1]
		list_type_2 = t_2[1]
		return is_same_type(list_type_1, list_type_2, tv_bindings)

if __name__ == "__main__":
	import ipdb
	ipdb.set_trace()
	print is_func_matches_args(inc_t, [num])