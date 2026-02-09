import ctypes
import math
import random
import sys
import time
from pathlib import Path

try:
    from ursina import (
        Entity,
        Grid,
        Sky,
        Text,
        Ursina,
        camera,
        color,
        destroy,
        quit,
        window,
    )
except ImportError:
    print("Error: Ursina is not installed.")
    print("Install it with: python3 -m pip install ursina")
    sys.exit(1)


STATUS_SOURCE_MASK = 0x0000000F
STATUS_SOURCE_NONE = 0x00000000
STATUS_SOURCE_IIO = 0x00000001
STATUS_SOURCE_SIM = 0x00000002
STATUS_FLAG_SENSOR_MISSING = 0x00000100


def source_name(flags: int) -> str:
    source = flags & STATUS_SOURCE_MASK
    if source == STATUS_SOURCE_IIO:
        return "IIO"
    if source == STATUS_SOURCE_SIM:
        return "SIM"
    return "NONE"


class Sensor_Sample_t(ctypes.Structure):
    _fields_ = [
        ("timestamp_ns", ctypes.c_uint64),
        ("accel_x", ctypes.c_int32),
        ("accel_y", ctypes.c_int32),
        ("accel_z", ctypes.c_int32),
        ("gyro_x", ctypes.c_int32),
        ("gyro_y", ctypes.c_int32),
        ("gyro_z", ctypes.c_int32),
        ("mag_x", ctypes.c_int32),
        ("mag_y", ctypes.c_int32),
        ("mag_z", ctypes.c_int32),
        ("status_flags", ctypes.c_uint32),
        ("padding", ctypes.c_uint8 * 16),
    ]


RING_BUFFER_SIZE = 1024


class Shared_Memory_Area_t(ctypes.Structure):
    _fields_ = [
        ("write_index", ctypes.c_uint32),
        ("read_index", ctypes.c_uint32),
        ("write_count", ctypes.c_uint64),
        ("writer_pid", ctypes.c_uint32),
        ("reserved0", ctypes.c_uint32),
        ("buffer", Sensor_Sample_t * RING_BUFFER_SIZE),
    ]


repo_root = Path(__file__).resolve().parent.parent
lib_path = repo_root / "bin" / "libdriver.so"
try:
    c_driver = ctypes.CDLL(str(lib_path))
except OSError:
    print(f"Error: Could not load {lib_path}")
    print("Build it first with: make")
    sys.exit(1)

c_driver.ggs_map_shared_memory.restype = ctypes.c_void_p
shared_ptr = c_driver.ggs_map_shared_memory()
if not shared_ptr:
    print("Error: Could not map shared memory.")
    sys.exit(1)

global_memory = ctypes.cast(shared_ptr, ctypes.POINTER(Shared_Memory_Area_t))
c_driver.start_c_driver()

app = Ursina(title="GALACTIC INS - TACTICAL VISUALIZER", borderless=False)
window.color = color.black
Sky(color=color.black)

for _ in range(400):
    Entity(
        model="sphere",
        color=color.random_color(),
        scale=random.uniform(0.05, 0.15),
        position=(random.randint(-100, 100), random.randint(-50, 50), random.randint(-100, 100)),
    )

Entity(model=Grid(100, 100), rotation_x=90, scale=200, color=color.rgba(0, 255, 255, 40))

laptop_pivot = Entity()
base = Entity(parent=laptop_pivot, model="cube", color=color.gray, scale=(6, 0.4, 4))
Entity(parent=base, model="cube", color=color.black, scale=(0.9, 0.1, 0.8), y=0.5)

hinge = Entity(parent=laptop_pivot, y=0.2, z=2.0)
screen_frame = Entity(parent=hinge, model="cube", color=color.dark_gray, scale=(6, 4, 0.2), origin_y=-0.5)
screen_frame.rotation_x = -110

display = Entity(parent=screen_frame, model="quad", color=color.cyan, scale=(0.95, 0.9), z=-0.55, origin_y=-0.5)
display.alpha = 0.6
Text(parent=display, text="GALACTIC-INS\nSYSTEM: LIVE\nSTATE: NOMINAL", scale=10, x=-0.4, y=0.8, color=color.black)

hud_text = Text(text="[ GALACTIC-INS ]\nWAITING FOR DATA...", position=(-0.86, 0.40), scale=1.35, color=color.cyan)

current_read = int(global_memory.contents.write_index)
cycle_count = 0
start_wall = time.monotonic()
last_sample_wall = start_wall
last_write_count = int(global_memory.contents.write_count)
takeover_attempted = False
fatal_error = None


def update():
    global current_read, cycle_count, last_sample_wall, last_write_count, takeover_attempted, fatal_error

    display.alpha = 0.5 + (math.sin(time.time() * 8) * 0.2)
    write_idx = int(global_memory.contents.write_index)
    write_count = int(global_memory.contents.write_count)

    if last_write_count and write_count > last_write_count + RING_BUFFER_SIZE:
        current_read = write_idx
        last_write_count = write_count

    if current_read != write_idx:
        sample = global_memory.contents.buffer[current_read]
        flags = int(sample.status_flags)
        src = source_name(flags)
        if (flags & STATUS_FLAG_SENSOR_MISSING) and src == "NONE":
            fatal_error = "No real sensor found (set GGS_ALLOW_SIM=1 only for fallback)."
            return

        acc_scale = 0.00008
        gyro_scale = 0.08

        laptop_pivot.rotation_x += sample.gyro_x * gyro_scale
        laptop_pivot.rotation_y += sample.gyro_y * gyro_scale
        laptop_pivot.rotation_z += sample.gyro_z * gyro_scale

        laptop_pivot.x += sample.accel_x * acc_scale
        laptop_pivot.y += sample.accel_z * acc_scale
        laptop_pivot.z += sample.accel_y * acc_scale

        if cycle_count % 10 == 0:
            hud_text.text = (
                f"[ GALACTIC-INS ] SRC:{src}\n"
                f"X:{sample.accel_x:>7} mm/s^2\n"
                f"Y:{sample.accel_y:>7} mm/s^2\n"
                f"Z:{sample.accel_z:>7} mm/s^2"
            )

        if cycle_count % 4 == 0:
            trail = Entity(model="cube", color=color.cyan, scale=0.15, position=laptop_pivot.position)
            trail.animate_scale(0, duration=1.2)
            trail.animate_color(color.clear, duration=1.2)
            destroy(trail, delay=1.2)

        current_read = (current_read + 1) % RING_BUFFER_SIZE
        cycle_count += 1
        last_sample_wall = time.monotonic()
        last_write_count = write_count
        takeover_attempted = False

    now = time.monotonic()
    if cycle_count == 0 and (now - start_wall) > 3.0:
        if not takeover_attempted:
            print("WARN: no samples in 3 seconds, attempting writer takeover.")
            c_driver.start_c_driver()
            time.sleep(0.05)
            current_read = int(global_memory.contents.write_index)
            last_write_count = int(global_memory.contents.write_count)
            takeover_attempted = True
            start_wall = now
        elif (now - start_wall) > 6.0:
            fatal_error = "No samples received in 6 seconds."
    elif cycle_count > 0 and (now - last_sample_wall) > 2.0:
        if not takeover_attempted:
            print("WARN: sample stream stalled, attempting writer takeover.")
            c_driver.start_c_driver()
            time.sleep(0.05)
            current_read = int(global_memory.contents.write_index)
            last_write_count = int(global_memory.contents.write_count)
            takeover_attempted = True
            last_sample_wall = now
        elif (now - last_sample_wall) > 4.0:
            fatal_error = "Sample stream stalled for 4 seconds."

    if fatal_error is not None:
        hud_text.text = f"[ GALACTIC-INS ]\nERROR:\n{fatal_error}"
        print(f"ERROR: {fatal_error}")
        quit()


camera.position = (15, 10, -25)
camera.look_at(laptop_pivot)


def input(key):
    if key == "escape":
        quit()


app.run()
