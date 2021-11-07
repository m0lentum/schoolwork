import simpy as sp

env = sp.Environment()


# resource setup


PREP_ROOM_COUNT = 3
PREP_ROOM_CAPACITY = 5

OPERATING_ROOM_CAPACITY = 6

RECOVERY_ROOM_COUNT = 2
RECOVERY_ROOM_CAPACITY = 8

# assuming we can always send a patient to any preparation room,
# we can treat all rooms as a single resource with the combined capacity of all rooms
prep_rooms = sp.Resource(env, capacity=PREP_ROOM_CAPACITY * PREP_ROOM_COUNT)
operating_room = sp.Resource(env, capacity=OPERATING_ROOM_CAPACITY)
recovery_rooms = sp.Resource(env, capacity=RECOVERY_ROOM_CAPACITY * RECOVERY_ROOM_COUNT)


# process setup


def patient(env, prep_rooms, operating_room, recovery_rooms):
    # durations of the different stages of the process
    # TODO: random exponential distribution
    def prep_duration():
        return 40

    def operation_duration():
        return 20

    def recovery_duration():
        return 40

    # the process itself
    with prep_rooms.request() as wait_for_prep_room:
        yield wait_for_prep_room
        yield env.timeout(prep_duration())

    with operating_room.request() as wait_for_op_room:
        yield wait_for_op_room
        yield env.timeout(operation_duration())

    with recovery_rooms.request() as wait_for_recovery_room:
        yield wait_for_recovery_room
        yield env.timeout(recovery_duration())


# generator process to bring new patients into the system
def patient_flow(env, prep_rooms, operating_room, recovery_rooms):
    def inter_arrival_delay():
        return 10

    while True:
        env.process(patient(env, prep_rooms, operating_room, recovery_rooms))
        yield env.timeout(inter_arrival_delay())


# run the simulation


env.process(patient_flow(env, prep_rooms, operating_room, recovery_rooms))

# run the simulation loop manually to inspect state for statistics
until_time = 500
for t in range(1, until_time):
    env.run(until=t)
    # simple periodic printout of simulation state for testing.
    # in the future, collect desired statistics and maybe draw a plot with matplotlib
    if t % 20 == 0:
        print("--------------------------------------------------")
        print(f"t = {t}")
        print(f"preparation spots taken: {prep_rooms.count}/{prep_rooms.capacity}")
        print(
            f"operation spots taken: {operating_room.count}/{operating_room.capacity}"
        )
        print(f"recovery spots taken: {recovery_rooms.count}/{recovery_rooms.capacity}")
