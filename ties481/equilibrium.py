# experiment computing serial correlation for assigment 4.

from hospital import run_simulation, TimeDistributions

# reuse mean and variance computation from previous assignment
from runner import compute_vals_of_interest

from math import sqrt
from typing import List

# fewest rooms and fastest arrival rate seems most likely to build up long queues,
# leading to serial correlation. Thus, pick the case 4p4r and interarrival distribution exp(22.5)
PREP_ROOM_COUNT = 4
REC_ROOM_COUNT = 4
ARRIVAL_DISTR = lambda rng: rng.exponential(scale=22.5)

SIM_TIME = 1000
RUN_COUNT = 10


def serial_correlation(time_series: List[int]) -> float:
    # compute autocorrelation for d = SAMPLE_FREQ
    # (source: lecture 6 slides which I hopefully haven't completely misunderstood)
    series_starting_at_d = time_series[1:]
    voi_orig = compute_vals_of_interest(time_series)
    voi_shifted = compute_vals_of_interest(series_starting_at_d)
    if voi_orig.variance == 0 or voi_shifted.variance == 0:
        # sometimes we get runs where the queue is empty the entire time,
        # making variance zero
        return 0.0
    covariance_samples = [
        (xi - voi_orig.mean) * (xid - voi_shifted.mean)
        for xi, xid in zip(time_series, series_starting_at_d)
    ]
    covariance = compute_vals_of_interest(covariance_samples).mean
    return covariance / sqrt(voi_orig.variance * voi_shifted.variance)


def test_sample_frequency(sample_freq: float):
    distr = TimeDistributions()
    distr.inter_arrival_delay = ARRIVAL_DISTR
    samples = [
        run_simulation(
            PREP_ROOM_COUNT,
            REC_ROOM_COUNT,
            SIM_TIME,
            sample_freq,
            rng_seed=None,
            distributions=distr,
        )
        for _ in range(RUN_COUNT)
    ]
    prep_room_correlations = [
        serial_correlation(s.prep_room_queue_samples) for s in samples
    ]
    rec_room_correlations = [
        serial_correlation(s.recovery_room_queue_samples) for s in samples
    ]

    print(f"sample frequency: {sample_freq}")
    print("serial correlation of preparation room queue:")
    compute_vals_of_interest(prep_room_correlations).print()
    print("serial correlation of recovery room queue:")
    compute_vals_of_interest(rec_room_correlations).print()


if __name__ == "__main__":
    TEST_FREQUENCIES = [5, 10, 20, 50, 100, 200]
    for freq in TEST_FREQUENCIES:
        test_sample_frequency(freq)
        print("")
