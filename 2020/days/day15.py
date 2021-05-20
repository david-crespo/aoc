import os

SAMPLES = ["0,3,6", "1,3,2", "2,1,3", "1,2,3"]


def run(inp, N=2020):
    last_spoken = {}
    nums = list(map(int, inp.split(",")))
    for i, n in enumerate(nums[:-1]):
        last_spoken[n] = i + 1

    i = len(nums)
    last = nums[-1]
    while i < N:
        new_num = i - last_spoken[last] if last in last_spoken else 0
        last_spoken[last] = i
        last = new_num
        i += 1

    print(last)


if __name__ == "__main__":
    os.system("clear")
    inp = "0,1,5,10,3,12,19"

    print("=============== A ===============\n")
    for s in SAMPLES:
        run(s)
    print("\n" + "-" * 15 + "\n")
    run(inp)

    print("\n=============== B ===============\n")
    N = 30000000
    run(SAMPLES[0], N)
    print("\n" + "-" * 15 + "\n")
    run(inp, N)
