def run_for_inputs(inputs, run, label=None):
    if label:
        print(f"=============== {label} ===============\n")
    for i, inp in enumerate(inputs):
        run(inp)
        print()
        if i < len(inputs) - 1:
            print("-" * 15)
            print()


def count(pred_or_value, iterable):
    pred = pred_or_value if callable(pred_or_value) else lambda x: x == pred_or_value
    return sum(1 for i in filter(pred, iterable))


def consec_pairs(lst):
    return zip(lst, lst[1:])


# make normal lambdas accept any number of args by only passing
# in the number of args they actually want and ignoring the rest.
# if the lambda wants more than we have, pass in None for missing args
def variadic(f):
    def new_f(*args):
        arg_count = f.__code__.co_argcount
        # chop off args or pad with None as appropriate
        new_args = (list(args) + [None] * arg_count)[:arg_count]
        return f(*new_args)

    return new_f
