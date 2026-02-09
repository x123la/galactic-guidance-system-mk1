import ctypes
import time
import os
import math

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
        ("buffer",      Sensor_Sample_t * RING_BUFFER_SIZE)
    ]

# --- 2. Load the C Driver ---
lib_path = os.path.abspath("gemini_ins/bin/libdriver.so")
try:
    c_driver = ctypes.CDLL(lib_path)
except OSError:
    print(f"Error: Could not load {lib_path}")
    exit(1)

# Bind to the global shared memory symbol
global_memory = Shared_Memory_Area_t.in_dll(c_driver, "global_shared_memory")

# --- 3. The "Ada Brain" Logic (Python Version) ---
print("==================================================")
print("   GEMINI INERTIAL NAVIGATION SYSTEM (DEMO)       ")
print("==================================================")
print("Architecture: Zero-Copy Hyper-Plane (Python + C)")
print("Initializing Hardware Pump...")

c_driver.start_c_driver()
time.sleep(0.1) # Prime the pump

print("Entering Real-Time Navigation Loop...")

current_read = 0
cycle_count = 0

try:
    while True:
        # Atomic Load (Simulated)
        write_idx = global_memory.write_index
        
        if current_read != write_idx:
            # Zero-Copy Fetch
            sample = global_memory.buffer[current_read]
            
            # Physics Conversion (Ada Style)
            accel_scale = 9.80665 / 1000.0
            acc_z_mps2 = sample.accel_z * accel_scale
            
            # Telemetry (Every 40th sample ~ 10Hz)
            if cycle_count % 40 == 0:
                print(f"[{sample.timestamp_ns}] ACC_Z: {acc_z_mps2:.4f} m/s^2 | GYRO_Z: {sample.gyro_z} | MAG_X: {sample.mag_x}")
            
            cycle_count += 1
            
            # Update Read Index
            current_read = (current_read + 1) % RING_BUFFER_SIZE
            global_memory.read_index = current_read
            
        else:
            time.sleep(0.0001) # Busy wait
except KeyboardInterrupt:
    print("\nShutting down INS.")
