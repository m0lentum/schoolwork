from hospital import run_simulation, TrackedStats

import numpy as np
from math import sqrt
from typing import List


SIM_TIME = 1000
SAMPLE_COUNT = 20

# use same rng seeds for each configuration for pairwise comparison.
# generate the seeds randomly to see if results are roughly consistent between different seeds
rng = np.random.default_rng()
seeds = rng.integers(low=0, high=10000, size=SAMPLE_COUNT)
stats_case_3p4r = [run_simulation(3, 4, SIM_TIME, seed) for seed in seeds]
stats_case_3p5r = [run_simulation(3, 5, SIM_TIME, seed) for seed in seeds]
stats_case_4p5r = [run_simulation(4, 5, SIM_TIME, seed) for seed in seeds]


class ValuesOfInterest:
    mean: float
    conf_ival_95_size: float

    def __init__(self, mean, conf_ival_95_size):
        self.mean = mean
        self.conf_ival_95_size = conf_ival_95_size

    def print(self):
        print(f"mean: {self.mean}")
        conf_min = self.mean - self.conf_ival_95_size
        conf_max = self.mean + self.conf_ival_95_size
        print(f"95% confidence interval: [{conf_min}, {conf_max}]")


def compute_vals_of_interest(samples: List[float]) -> ValuesOfInterest:
    """Compute mean and 95% confidence interval for an array of float-valued samples."""

    mean = sum(samples) / SAMPLE_COUNT
    variance = sum([(p - mean) ** 2 for p in samples]) / (SAMPLE_COUNT - 1)
    std_deviation = sqrt(variance)
    # I don't know how to analyze which distribution the samples obey here
    # and I don't have time to study this,
    # so I'm guessing it's the normal distribution like in lecture slides.
    # Following https://en.wikipedia.org/wiki/Confidence_interval#Example,

    # value of the t distribution's 97.5th percentile at DOF 20.
    # this will need to be changed if sample count changes
    c = 2.093
    return ValuesOfInterest(
        mean=mean,
        conf_ival_95_size=c * std_deviation / sqrt(SAMPLE_COUNT),
    )


def analyze(samples: List[TrackedStats]):
    """Compute and print ValuesOfInterest for every statistic we're interested in."""

    print("Probability of all recovery rooms being full:")
    compute_vals_of_interest(
        [s.time_all_recovery_rooms_full / SIM_TIME for s in samples]
    ).print()
    print("Average queue length before preparation:")
    compute_vals_of_interest(
        [s.prep_room_queue_length_total / SIM_TIME for s in samples]
    ).print()


print("individual cases:")
print("\n3p4r")
analyze(stats_case_3p4r)
print("\n3p5r")
analyze(stats_case_3p5r)
print("\n4p5r")
analyze(stats_case_4p5r)
print("\npairwise differences:")
print("\nfrom 3p4r to 3p5r:")
analyze([s2.difference_from(s1) for s1, s2 in zip(stats_case_3p4r, stats_case_3p5r)])
print("\nfrom 3p4r to 4p5r:")
analyze([s2.difference_from(s1) for s1, s2 in zip(stats_case_3p4r, stats_case_4p5r)])
print("\nfrom 3p5r to 4p5r:")
analyze([s2.difference_from(s1) for s1, s2 in zip(stats_case_3p5r, stats_case_4p5r)])
