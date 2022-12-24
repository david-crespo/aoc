import os
from collections import namedtuple
import re
from typing import List, Tuple, Dict
from functools import cache
import cProfile
import time

DAY = os.path.basename(__file__)[3:5]
with open(f"input/day{DAY}.txt") as f:
    real_input = f.read().strip()

with open(f"input/day{DAY}-example.txt") as f:
    sample_input = f.read().strip()


Blueprint = namedtuple(
    "Blueprint",
    [
        "i",
        "ore_cost_ore",
        "clay_cost_ore",
        "obs_cost_ore",
        "obs_cost_clay",
        "geo_cost_ore",
        "geo_cost_obs",
    ],
)

State = namedtuple(
    "State",
    [
        "ore",
        "ore_robots",
        "clay",
        "clay_robots",
        "obs",
        "obs_robots",
        "geo",
        "geo_robots",
    ],
)


def parse_line(s: str) -> Blueprint:
    nums = [int(n) for n in re.findall("[0-9]+", s)]
    return Blueprint(*nums)


@cache
def next_states(bp: Blueprint, state: State, ahead: int) -> List[State]:
    if ahead == 1:
        new_ore = state.ore + state.ore_robots
        new_clay = state.clay + state.clay_robots
        new_obs = state.obs + state.obs_robots
        new_geo = state.geo + state.geo_robots

        only_mine = state._replace(
            ore=new_ore,
            clay=new_clay,
            obs=new_obs,
            geo=new_geo,
        )

        ns = [only_mine]

        if state.ore >= bp.ore_cost_ore:
            ns.append(
                only_mine._replace(
                    ore_robots=state.ore_robots + 1, ore=new_ore - bp.ore_cost_ore
                )
            )

        if state.ore >= bp.clay_cost_ore:
            ns.append(
                only_mine._replace(
                    clay_robots=state.clay_robots + 1, ore=new_ore - bp.clay_cost_ore
                )
            )

        if state.ore >= bp.obs_cost_ore and state.clay >= bp.obs_cost_clay:
            ns.append(
                only_mine._replace(
                    obs_robots=state.obs_robots + 1,
                    ore=new_ore - bp.obs_cost_ore,
                    clay=new_clay - bp.obs_cost_clay,
                )
            )

        if state.ore >= bp.geo_cost_ore and state.obs >= bp.geo_cost_obs:
            ns.append(
                only_mine._replace(
                    geo_robots=state.geo_robots + 1,
                    ore=new_ore - bp.geo_cost_ore,
                    obs=new_obs - bp.geo_cost_obs,
                )
            )

        return ns
    if ahead == 2:
        return [nn for n in next_states(bp, state, 1) for nn in next_states(bp, n, 1)]
    elif ahead == 3:
        return [nnn for n in next_states(bp, state, 1) for nnn in next_states(bp, n, 2)]
    elif ahead == 4:
        return [
            nnnn for nn in next_states(bp, state, 2) for nnnn in next_states(bp, nn, 2)
        ]
    return []


@cache
def most_geodes(bp: Blueprint, state: State, mins_left: int) -> State:
    if mins_left == 0:
        return state

    if mins_left == 1:
        return max(next_states(bp, state), key=lambda st: st.geo)

    if mins_left == 2:
        return max(next_states(bp, state, 2), key=lambda st: st.geo)

    if mins_left == 3:
        return max(next_states(bp, state, 3), key=lambda st: st.geo)

    if mins_left == 4:
        return max(next_states(bp, state, 4), key=lambda st: st.geo)

    return max(
        (most_geodes(bp, s, mins_left - 4) for s in next_states(bp, state, 4)),
        key=lambda st: st.geo,
    )


init_state = State(0, 1, 0, 0, 0, 0, 0, 0)


def run(input):
    start = time.time()
    bps = [parse_line(line) for line in input.split("\n")]
    # print(most_geodes(bps[0], init_state, 24))
    x = 0
    for bp in bps:
        print(f"{bp.i}: ", end="", flush=True)
        s = most_geodes(bp, init_state, 20)
        print(f"{s.geo}")
        x += bp.i * s.geo
    end = time.time()
    print(f"total: {x} ({round(end-start, 1)}s)")


# 2136 too low
# took a fucking hour to run


def run2(input):
    print(2)


print("=" * 40)
run(sample_input)
# cProfile.run("run(sample_input)")
# run(real_input)

# print()
# run2(sample_input)
# run2(real_input)
