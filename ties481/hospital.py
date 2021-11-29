import simpy as sp
import numpy as np
from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class TrackedStats:
    # totals for assignment 3
    time_all_recovery_rooms_full = 0
    patient_lifetime_total = 0
    patient_count = 0
    failed_operation_count = 0
    prep_room_queue_length_total = 0
    # time series sampling for assignment 4
    prep_room_queue_samples: List[int] = field(default_factory=list)
    recovery_room_queue_samples: List[int] = field(default_factory=list)

    def difference_from(self, other):
        """Compute the difference between two simulation runs for pairwise comparison.

        Does not handle time series samples as these weren't used in Assignment 4."""

        diff = TrackedStats()
        diff.time_all_recovery_rooms_full = (
            self.time_all_recovery_rooms_full - other.time_all_recovery_rooms_full
        )
        diff.patient_lifetime_total = (
            self.patient_lifetime_total - other.patient_lifetime_total
        )
        diff.patient_count = self.patient_count - other.patient_count
        diff.failed_operation_count = (
            self.failed_operation_count - other.failed_operation_count
        )
        diff.prep_room_queue_length_total = (
            self.prep_room_queue_length_total - other.prep_room_queue_length_total
        )
        return diff


@dataclass
class TimeDistributions:
    """Distributions of durations of parts of the patient process."""

    # for some reason these implicitly take the TimeDistributions object as `self`
    # even though they're not defined as member functions
    inter_arrival_delay = lambda self, rng: rng.exponential(scale=25)
    prep_duration = lambda self, rng: rng.exponential(scale=40)
    operation_duration = lambda self, rng: rng.exponential(scale=20)
    recovery_duration = lambda self, rng: rng.exponential(scale=40)
    retry_delay = lambda self, rng: rng.exponential(scale=5)


def run_simulation(
    prep_room_count: int,
    recovery_room_count: int,
    until_time: int,
    sample_frequency: int,
    rng_seed: Optional[int],
    distributions=TimeDistributions(),
    apply_twist: bool = True,
) -> TrackedStats:
    env = sp.Environment()
    rng = np.random.default_rng(seed=rng_seed)

    # resource setup and parameters

    PREP_ROOM_CAPACITY = 1
    OPERATING_ROOM_CAPACITY = 1
    RECOVERY_ROOM_CAPACITY = 1

    # twist: operations can fail and need to be redone
    FAILURE_PROBABILITY = 0.1

    # assuming we can always send a patient to any preparation room,
    # we can treat all rooms as a single resource with the combined capacity of all rooms
    prep_rooms = sp.Resource(env, capacity=PREP_ROOM_CAPACITY * prep_room_count)
    operating_room = sp.Resource(env, capacity=OPERATING_ROOM_CAPACITY)
    recovery_rooms = sp.Resource(
        env, capacity=RECOVERY_ROOM_CAPACITY * recovery_room_count
    )

    stats = TrackedStats()

    # process setup

    def patient(env, prep_rooms, operating_room, recovery_rooms, stats):
        prep_duration = distributions.prep_duration(rng)
        operation_duration = distributions.operation_duration(rng)
        recovery_duration = distributions.recovery_duration(rng)
        retry_delay = distributions.retry_delay(rng)

        # keep track of time for statistics purposes
        start_time = env.now
        # the process itself
        # loop is for redoing failed operations
        while True:
            # manually making and releasing requests instead of using `with` contexts
            # to simulate the patient staying in the previous room
            # until a spot opens up in the next one
            in_prep_room = prep_rooms.request()
            yield in_prep_room
            yield env.timeout(prep_duration)

            in_op_room = operating_room.request()
            yield in_op_room
            prep_rooms.release(in_prep_room)
            yield env.timeout(operation_duration)

            in_rec_room = recovery_rooms.request()
            yield in_rec_room
            operating_room.release(in_op_room)
            yield env.timeout(recovery_duration)
            recovery_rooms.release(in_rec_room)

            if apply_twist and rng.random() < FAILURE_PROBABILITY:
                stats.failed_operation_count += 1
                yield env.timeout(retry_delay)
            else:
                duration = env.now - start_time
                stats.patient_lifetime_total += duration
                stats.patient_count += 1
                return

    # generator process to bring new patients into the system
    def patient_flow(env, prep_rooms, operating_room, recovery_rooms, stats):
        while True:
            env.process(patient(env, prep_rooms, operating_room, recovery_rooms, stats))
            yield env.timeout(distributions.inter_arrival_delay(rng))

    # run the simulation

    env.process(patient_flow(env, prep_rooms, operating_room, recovery_rooms, stats))

    # run the simulation loop manually to be able to inspect state
    for t in range(1, until_time):
        env.run(until=t)

        stats.prep_room_queue_length_total += len(prep_rooms.queue)

        if recovery_rooms.count == recovery_rooms.capacity:
            stats.time_all_recovery_rooms_full += 1

        if t % sample_frequency == 0:
            stats.prep_room_queue_samples.append(len(prep_rooms.queue))
            stats.recovery_room_queue_samples.append(len(recovery_rooms.queue))

    return stats


# if this file is run from the command line, run the simulation once.
# proper analysis with multiple runs is done in other modules
if __name__ == "__main__":
    stats = run_simulation(
        prep_room_count=4,
        recovery_room_count=3,
        sample_frequency=20,
        until_time=1000,
        rng_seed=None,
    )
    print(stats.__dict__)
    print(stats.prep_room_queue_samples)
    print(stats.recovery_room_queue_samples)
