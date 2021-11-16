from hospital import run_simulation, TrackedStats

from math import sqrt
from typing import List


SIM_TIME = 1000
SAMPLE_COUNT = 20

stats_case_3p4r = [run_simulation(3, 4, SIM_TIME) for _ in range(SAMPLE_COUNT)]
stats_case_3p5r = [run_simulation(3, 5, SIM_TIME) for _ in range(SAMPLE_COUNT)]
stats_case_4p5r = [run_simulation(4, 5, SIM_TIME) for _ in range(SAMPLE_COUNT)]


class ValuesOfInterest:
    mean: float
    conf_ival_95_size: float

    def __init__(self, mean, conf_ival_95_size):
        self.mean = mean
        self.conf_ival_95_size = conf_ival_95_size

    def print(self):
        print(f"mean: {self.mean}")
        print(
            f"95% confidence interval: [{self.mean - self.conf_ival_95_size}, {self.mean + self.conf_ival_95_size}]"
        )


def analyze_samples(samples: List[float]) -> ValuesOfInterest:
    """Compute mean and 95% confidence interval for an array of float-valued samples."""

    mean = sum(samples) / SAMPLE_COUNT
    variance = sum([(p - mean) ** 2 for p in samples]) / (SAMPLE_COUNT - 1)
    std_deviation = sqrt(variance)
    # I don't know how to analyze which distribution the samples obey here
    # and I don't have time to study this,
    # so I'm guessing it's the normal distribution like in lecture slides.
    # Following https://en.wikipedia.org/wiki/Confidence_interval#Example,
    c = 2.093  # this will need to be changed if sample count changes
    return ValuesOfInterest(
        mean=mean,
        conf_ival_95_size=c * std_deviation / sqrt(SAMPLE_COUNT),
    )


def analyze_all(samples: List[TrackedStats]):
    """Compute and print ValuesOfInterest for every statistic we're interested in."""

    print("Probability of all recovery rooms being full:")
    analyze_samples(
        [s.time_all_recovery_rooms_full / SIM_TIME for s in samples]
    ).print()
    print("Average queue length before preparation:")
    analyze_samples(
        [s.prep_room_queue_length_total / SIM_TIME for s in samples]
    ).print()


print("case 3p4r")
analyze_all(stats_case_3p4r)
print("\ncase 3p5r")
analyze_all(stats_case_3p5r)
print("\ncase 4p5r")
analyze_all(stats_case_4p5r)
