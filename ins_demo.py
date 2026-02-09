import argparse
import ctypes
import sys
import time
from pathlib import Path

# --- 1. Define the Shared Memory Structure (Matches sensor_protocol.h) ---
class Sensor_Sample_t(ctypes.Structure):
    _fields_ = [
        ("timestamp_ns", ctypes.c_uint64),
        ("accel_x",      ctypes.c_int32),
        ("accel_y",      ctypes.c_int32),
        ("accel_z",      ctypes.c_int32),
        ("gyro_x",       ctypes.c_int32),
        ("gyro_y",       ctypes.c_int32),
        ("gyro_z",       ctypes.c_int32),
        ("mag_x",        ctypes.c_int32),
        ("mag_y",        ctypes.c_int32),
        ("mag_z",        ctypes.c_int32),
        ("status_flags", ctypes.c_uint32),
        ("padding",      ctypes.c_uint8 * 16)
    ]

RING_BUFFER_SIZE = 1024

class Shared_Memory_Area_t(ctypes.Structure):
    _fields_ = [
        ("write_index", ctypes.c_uint32),
        ("read_index",  ctypes.c_uint32),
        ("write_count", ctypes.c_uint64),
        ("writer_pid",  ctypes.c_uint32),
        ("reserved0",   ctypes.c_uint32),
        ("buffer",      Sensor_Sample_t * RING_BUFFER_SIZE)
    ]

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


def main() -> int:
    parser = argparse.ArgumentParser(description="Run the Galactic INS Python demo.")
    parser.add_argument(
        "--seconds",
        type=float,
        default=0.0,
        help="Runtime limit in seconds (0 = run forever).",
    )
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parent
    lib_path = repo_root / "bin" / "libdriver.so"
    try:
        c_driver = ctypes.CDLL(str(lib_path))
    except OSError:
        print(f"Error: Could not load {lib_path}")
        print("Build it first with: make")
        return 1

    print("==================================================")
    print("   GALACTIC INERTIAL NAVIGATION SYSTEM (DEMO)     ")
    print("==================================================")
    print("Architecture: Zero-Copy Hyper-Plane (Python + C)")
    print("Status: real sensor required unless GGS_ALLOW_SIM=1")
    print("Initializing Hardware Pump...")

    c_driver.ggs_map_shared_memory.restype = ctypes.c_void_p
    shared_ptr = c_driver.ggs_map_shared_memory()
    if not shared_ptr:
        print("ERROR: Could not map shared memory.")
        return 1

    global_memory = ctypes.cast(shared_ptr, ctypes.POINTER(Shared_Memory_Area_t))
    c_driver.start_c_driver()
    time.sleep(0.1)
    print("Entering Real-Time Navigation Loop...")

    current_read = int(global_memory.contents.write_index)
    cycle_count = 0
    start = time.monotonic()
    startup_window_start = start
    last_sample_wall = start
    last_write_count = int(global_memory.contents.write_count)
    takeover_attempted = False

    try:
        while True:
            write_idx = int(global_memory.contents.write_index)
            write_count = int(global_memory.contents.write_count)

            if last_write_count and write_count > last_write_count + RING_BUFFER_SIZE:
                print("WARN: reader lag detected, resyncing to live head.")
                current_read = write_idx
                last_write_count = write_count

            if current_read != write_idx:
                sample = global_memory.contents.buffer[current_read]
                flags = int(sample.status_flags)
                src = source_name(flags)

                if (flags & STATUS_FLAG_SENSOR_MISSING) and src == "NONE":
                    print("ERROR: No real accelerometer source found.")
                    print("Hint: attach a Linux IIO sensor or set GGS_ALLOW_SIM=1 for simulation.")
                    return 1

                acc_z_mps2 = sample.accel_z / 1000.0
                if cycle_count % 40 == 0:
                    print(
                        f"[{sample.timestamp_ns}] SRC:{src} ACC_Z:{acc_z_mps2:.4f} m/s^2 "
                        f"GYRO_Z:{sample.gyro_z} MAG_X:{sample.mag_x}"
                    )

                cycle_count += 1
                current_read = (current_read + 1) % RING_BUFFER_SIZE
                last_sample_wall = time.monotonic()
                last_write_count = write_count
                takeover_attempted = False
            else:
                time.sleep(0.0005)

            now = time.monotonic()
            if cycle_count == 0 and (now - startup_window_start) > 3.0:
                if not takeover_attempted:
                    print("WARN: no samples in 3 seconds, attempting writer takeover.")
                    c_driver.start_c_driver()
                    time.sleep(0.05)
                    current_read = int(global_memory.contents.write_index)
                    last_write_count = int(global_memory.contents.write_count)
                    takeover_attempted = True
                elif (now - startup_window_start) > 6.0:
                    print("ERROR: No samples received in 6 seconds.")
                    return 1
            if args.seconds > 0 and (now - start) >= args.seconds:
                print("Run complete.")
                return 0
            if cycle_count > 0 and (now - last_sample_wall) > 2.0:
                if not takeover_attempted:
                    print("WARN: sample stream stalled, attempting writer takeover.")
                    c_driver.start_c_driver()
                    time.sleep(0.05)
                    current_read = int(global_memory.contents.write_index)
                    last_write_count = int(global_memory.contents.write_count)
                    takeover_attempted = True
                    last_sample_wall = now
                elif (now - last_sample_wall) > 4.0:
                    print("ERROR: Sample stream stalled for 4 seconds.")
                    return 1
    except KeyboardInterrupt:
        print("\nShutting down INS.")
        return 0


if __name__ == "__main__":
    sys.exit(main())
