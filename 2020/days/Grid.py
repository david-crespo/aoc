import util as u


def show(board, to_s=lambda c: str(c)):
    to_s = u.variadic(to_s)

    for j, row in enumerate(board):
        print("".join(to_s(cell, board, i, j) for i, cell in enumerate(row)))
    print()
