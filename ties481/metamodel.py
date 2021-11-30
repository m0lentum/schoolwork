# experiment building a metamodel for assignment 4.

# parameters:
# x0: interarrival time mean (-1 = 22.5, 1 = 25)
# x1: interarrival time distribution (-1 = exp, 1 = unif)
# x2: preparation time distribution (-1 = exp, 1 = unif)
# x3: recovery time distribution (-1 = exp, 1 = unif)
# x4: preparation unit count (-1 = 4, 1 = 5)
# x5: recovery unit count (-1 = 4, 1 = 5)
# all of these have two levels -> linear model.

# for 2^(6-3) model, select three joint effects to ignore.
# let's say x1x2x3 = x4x5x6 = x2x4x6 = 1.
# now the remaining cases are
# x0 | x1 | x2 | x3 | x4 | x5
# -1 |  1 |  1 |  1 |  1 | -1
#  1 |  1 | -1 |  1 |  1 | -1
#  1 | -1 |  1 | -1 | -1 | -1
# -1 | -1 | -1 | -1 | -1 | -1
#  1 | -1 |  1 |  1 | -1 |  1
# -1 |  1 |  1 | -1 |  1 |  1
#  1 |  1 | -1 | -1 |  1 |  1
# -1 | -1 | -1 |  1 | -1 |  1

import numpy as np
from hospital import run_simulation, TimeDistributions
from runner import compute_vals_of_interest

EXPERIMENT_CONFIGS = [
    [-1, 1, 1, 1, 1, -1],
    [1, 1, -1, 1, 1, -1],
    [1, -1, 1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1],
    [1, -1, 1, 1, -1, 1],
    [-1, 1, 1, -1, 1, 1],
    [1, 1, -1, -1, 1, 1],
    [-1, -1, -1, 1, -1, 1],
]

SIM_TIME = 1000
# in equilibrium.py tests it seemed that this is where
# serial correlation gets reasonably low
SAMPLE_FREQ = 50

# this is the vector `Y` in the regression model `Xb = Y`
prep_queue_avgs = []
for x in EXPERIMENT_CONFIGS:
    distr = TimeDistributions()
    if x[0] == 1:
        if x[1] == 1:
            distr.inter_arrival_delay = lambda rng: rng.uniform(20, 30)
        else:
            distr.inter_arrival_delay = lambda rng: rng.exponential(scale=25)
    else:
        if x[1] == 1:
            distr.inter_arrival_delay = lambda rng: rng.uniform(20, 25)
        else:
            distr.inter_arrival_delay = lambda rng: rng.exponential(scale=22.5)
    if x[2] == 1:
        distr.prep_duration = lambda rng: rng.uniform(30, 50)
    else:
        distr.prep_duration = lambda rng: rng.exponential(scale=40)
    if x[3] == 1:
        distr.recovery_duration = lambda rng: rng.uniform(30, 50)
    else:
        distr.recovery_duration = lambda rng: rng.exponential(scale=40)

    prep_room_count = 5 if x[4] == 1 else 4
    rec_room_count = 5 if x[5] == 1 else 4

    stats = run_simulation(
        prep_room_count,
        rec_room_count,
        SIM_TIME,
        SAMPLE_FREQ,
        rng_seed=None,
        distributions=distr,
    )
    prep_queue_avgs.append(compute_vals_of_interest(stats.prep_room_queue_samples).mean)

# fit linear model using least-squares from numpy
X = np.array(EXPERIMENT_CONFIGS, np.int32)
Y = np.array(prep_queue_avgs)
model = np.linalg.lstsq(X, Y, rcond=None)
print(f"model (beta): {model[0]}")
