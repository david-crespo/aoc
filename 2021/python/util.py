def count(pred_or_value, iterable):
    pred = pred_or_value if callable(pred_or_value) else lambda x: x == pred_or_value
    return sum(1 for i in filter(pred, iterable))


def pl(iter):
    for x in iter:
        print(x)
