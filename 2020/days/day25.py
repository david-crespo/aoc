import os


def transform(subj, loop):
    v = 1
    for i in range(loop):
        v = (v * subj) % 20201227
    return v


def get_loop(pub, subj):
    loop = 0
    v = 1
    while v != pub:
        v = (v * subj) % 20201227
        loop += 1
    return loop


def run(card_pub, door_pub):
    door_loop = get_loop(door_pub, 7)
    # print(door_loop)
    door_enc = transform(card_pub, door_loop)
    print(door_enc)


if __name__ == "__main__":
    os.system("clear")

    card_pub, door_pub = 12232269, 19452773
    sample_cp, sample_dp = 5764801, 17807724

    print("=============== A ===============\n")
    run(sample_cp, sample_dp)
    print("\n" + "-" * 15 + "\n")
    run(card_pub, door_pub)
