from aocd.models import Puzzle
import util as u
import re

TEST_INPUTS = [
    """
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
""",
    """
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
""",
    """
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
""",
]
TEST_INPUTS = [t.strip() for t in TEST_INPUTS]

REQ = set(["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"])

ECLS = set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])


def hgt_valid(v):
    if not re.match("^\d+(cm|in)$", v):
        return False

    num, unit = int(v[:-2]), v[-2:]

    if unit == "cm" and (num < 150 or num > 193):
        return False

    if unit == "in" and (num < 59 or num > 76):
        return False

    return True


VALIDATORS = {
    "byr": lambda v: len(v) == 4 and int(v) >= 1920 and int(v) <= 2002,
    "iyr": lambda v: len(v) == 4 and int(v) >= 2010 and int(v) <= 2020,
    "eyr": lambda v: len(v) == 4 and int(v) >= 2020 and int(v) <= 2030,
    "hgt": hgt_valid,
    "ecl": lambda v: v in ECLS,
    "pid": lambda v: re.match("^[0-9]{9}$", v),
    "hcl": lambda v: re.match("^#[0-9a-f]{6}$", v),
}


def parse(pp):
    return dict(tuple(field.split(":")) for field in pp)


def run_a(input_data):
    passports = input_data.split("\n\n")
    passports = [p.replace("\n", " ").split(" ") for p in passports]
    passports = list(map(parse, passports))
    print(u.count(lambda pp: REQ <= pp.keys(), passports))


def run_b(input_data):
    passports = input_data.split("\n\n")
    passports = [p.replace("\n", " ").split(" ") for p in passports]
    passports = list(map(parse, passports))

    def is_valid(pp):
        return REQ <= pp.keys() and all(VALIDATORS[k](pp[k]) for k in REQ)

    print(u.count(is_valid, passports))


if __name__ == "__main__":
    input_data = Puzzle(year=2020, day=4).input_data
    # u.run_for_inputs(TEST_INPUTS, run_a, "A")
    u.run_for_inputs(TEST_INPUTS + [input_data], run_a, "A")
    # u.run_for_inputs(TEST_INPUTS, run_b, "B")
    u.run_for_inputs(TEST_INPUTS + [input_data], run_b, "B")
