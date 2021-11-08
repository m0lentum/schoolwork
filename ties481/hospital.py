import simpy as sp
import numpy as np

env = sp.Environment()
rng = np.random.default_rng()


# resource setup and parameters (constants for now)


PREP_ROOM_COUNT = 3
PREP_ROOM_CAPACITY = 5

OPERATING_ROOM_CAPACITY = 6

RECOVERY_ROOM_COUNT = 2
RECOVERY_ROOM_CAPACITY = 5

# twist: operations can fail and need to be redone
FAILURE_PROBABILITY = 0.1

# assuming we can always send a patient to any preparation room,
# we can treat all rooms as a single resource with the combined capacity of all rooms
prep_rooms = sp.Resource(env, capacity=PREP_ROOM_CAPACITY * PREP_ROOM_COUNT)
operating_room = sp.Resource(env, capacity=OPERATING_ROOM_CAPACITY)
recovery_rooms = sp.Resource(env, capacity=RECOVERY_ROOM_CAPACITY * RECOVERY_ROOM_COUNT)


# statistics setup


class TrackedStats:
    time_all_recovery_rooms_full = 0
    patient_lifetime_total = 0
    patient_count = 0
    failed_operation_count = 0


stats = TrackedStats()


# process setup


def patient(env, prep_rooms, operating_room, recovery_rooms, stats):
    # durations of the different stages of the process
    def prep_duration():
        return 5 * rng.exponential() + 40

    def operation_duration():
        return 5 * rng.exponential() + 20

    def recovery_duration():
        return 5 * rng.exponential() + 60

    def retry_delay():
        return 5 * rng.exponential() + 5

    # keep track of time for statistics purposes
    start_time = env.now
    # the process itself
    # loop is for redoing failed operations
    while True:
        with prep_rooms.request() as wait_for_prep_room:
            yield wait_for_prep_room
            yield env.timeout(prep_duration())

        with operating_room.request() as wait_for_op_room:
            yield wait_for_op_room
            yield env.timeout(operation_duration())

        with recovery_rooms.request() as wait_for_recovery_room:
            yield wait_for_recovery_room
            yield env.timeout(recovery_duration())

        if rng.random() < FAILURE_PROBABILITY:
            stats.failed_operation_count += 1
            yield env.timeout(retry_delay())
        else:
            duration = env.now - start_time
            stats.patient_lifetime_total += duration
            stats.patient_count += 1
            return


# generator process to bring new patients into the system
def patient_flow(env, prep_rooms, operating_room, recovery_rooms, stats):
    def inter_arrival_delay():
        return 5 * rng.exponential() + 5

    while True:
        env.process(patient(env, prep_rooms, operating_room, recovery_rooms, stats))
        yield env.timeout(inter_arrival_delay())


# run the simulation


env.process(patient_flow(env, prep_rooms, operating_room, recovery_rooms, stats))

# run the simulation loop manually to be able to inspect state
until_time = 1000
for t in range(1, until_time):
    env.run(until=t)

    if recovery_rooms.count == recovery_rooms.capacity:
        stats.time_all_recovery_rooms_full += 1

    # periodically print out of simulation state for a bit of visualization.
    # in the future, maybe draw a plot with matplotlib
    if t % 50 == 0:
        print("--------------------------------------------------")
        print(f"t = {t}")
        print(f"preparation spots taken: {prep_rooms.count}/{prep_rooms.capacity}")
        print(
            f"operation spots taken: {operating_room.count}/{operating_room.capacity}"
        )
        print(f"recovery spots taken: {recovery_rooms.count}/{recovery_rooms.capacity}")

print("\n||================================================||")
print("||                    STATS                       ||")
print("||================================================||\n")
print(f"time all recovery rooms were full: {stats.time_all_recovery_rooms_full}")
print(f"patients treated: {stats.patient_count}")
avg_patient_lifetime = stats.patient_lifetime_total / stats.patient_count
print(f"average treatment duration: {avg_patient_lifetime}")
print(f"operation failed {stats.failed_operation_count} times")
