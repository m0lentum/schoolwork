import simpy as sp
import numpy as np
import sys

env = sp.Environment()
rng = np.random.default_rng()


# resource setup and parameters


# take room counts from the command line for easy configuration changes
PREP_ROOM_COUNT = int(sys.argv[1]) if len(sys.argv) >= 2 else 3
PREP_ROOM_CAPACITY = 1

OPERATING_ROOM_CAPACITY = 1

RECOVERY_ROOM_COUNT = int(sys.argv[2]) if len(sys.argv) >= 3 else 3
RECOVERY_ROOM_CAPACITY = 1

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
    prep_room_queue_length_total = 0


stats = TrackedStats()


# process setup


def patient(env, prep_rooms, operating_room, recovery_rooms, stats):
    # durations of the different stages of the process
    def prep_duration():
        # scale parameter corresponds to the mean of the distribution
        return rng.exponential(scale=40)

    def operation_duration():
        return rng.exponential(scale=20)

    def recovery_duration():
        return rng.exponential(scale=40)

    def retry_delay():
        return rng.exponential(scale=5)

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
        yield env.timeout(prep_duration())

        in_op_room = operating_room.request()
        yield in_op_room
        prep_rooms.release(in_prep_room)
        yield env.timeout(operation_duration())

        in_rec_room = recovery_rooms.request()
        yield in_rec_room
        operating_room.release(in_op_room)
        yield env.timeout(recovery_duration())
        recovery_rooms.release(in_rec_room)

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
        return rng.exponential(scale=25)

    while True:
        env.process(patient(env, prep_rooms, operating_room, recovery_rooms, stats))
        yield env.timeout(inter_arrival_delay())


# run the simulation


env.process(patient_flow(env, prep_rooms, operating_room, recovery_rooms, stats))

# run the simulation loop manually to be able to inspect state
until_time = 1000
for t in range(1, until_time):
    env.run(until=t)

    stats.prep_room_queue_length_total += len(prep_rooms.queue)

    if recovery_rooms.count == recovery_rooms.capacity:
        stats.time_all_recovery_rooms_full += 1


print(f"{PREP_ROOM_COUNT} preparation rooms with {PREP_ROOM_CAPACITY} spots each,")
print(f"{RECOVERY_ROOM_COUNT} recovery rooms with {RECOVERY_ROOM_CAPACITY} spots each.")
print("")
print(f"time all recovery rooms were full: {stats.time_all_recovery_rooms_full}")
print(f"patients treated: {stats.patient_count}")
avg_patient_lifetime = stats.patient_lifetime_total / stats.patient_count
print(f"average treatment duration: {avg_patient_lifetime}")
print(f"operation failed {stats.failed_operation_count} times")
avg_prep_queue = stats.prep_room_queue_length_total / until_time
print(f"average queue length before prep room: {avg_prep_queue}")
