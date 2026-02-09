from ursina import *
import ctypes
import os
import math
import random

# --- 1. Shared Memory Setup ---
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

lib_path = os.path.abspath("../bin/libdriver.so")
c_driver = ctypes.CDLL(lib_path)
global_memory = Shared_Memory_Area_t.in_dll(c_driver, "global_shared_memory")

# --- 2. 3D Engine Setup ---
app = Ursina(title="GALACTIC INS - TACTICAL VISUALIZER", borderless=False)

window.color = color.black
Sky(color=color.black)

# Enhanced Starfield
stars = []
for i in range(400):
    Entity(model='sphere', color=color.random_color(), 
           scale=random.uniform(0.05, 0.15),
           position=(random.randint(-100, 100), random.randint(-50, 50), random.randint(-100, 100)))

# Tactical Grid
grid = Entity(model=Grid(100, 100), rotation_x=90, scale=200, color=color.rgba(0, 255, 255, 40))

# --- 3. High-Fidelity Laptop Avatar ---
# Base Unit
laptop_pivot = Entity()
base = Entity(parent=laptop_pivot, model='cube', color=color.gray, scale=(6, 0.4, 4))
# Keyboard Area Detail
keyboard = Entity(parent=base, model='cube', color=color.black, scale=(0.9, 0.1, 0.8), y=0.5)

# Screen Hinge and Screen
hinge = Entity(parent=laptop_pivot, y=0.2, z=2.0)
screen_frame = Entity(parent=hinge, model='cube', color=color.dark_gray, scale=(6, 4, 0.2), origin_y=-0.5)
screen_frame.rotation_x = -110 # Open laptop angle

# Emissive Display
display = Entity(parent=screen_frame, model='quad', color=color.cyan, scale=(0.95, 0.9), z=-0.55, origin_y=-0.5)
display.alpha = 0.6
Text(parent=display, text="GALACTIC-INS\nSYSTEM: LIVE\nSTATE: NOMINAL", scale=10, x=-0.4, y=0.8, color=color.black)

# Data HUD
hud_text = Text(text="[ TELEMETRY ]\nX_ACC: 0\nY_ACC: 0\nZ_ACC: 0", position=(-0.85, 0.45), scale=1.5, color=color.cyan)

current_read = 0
cycle_count = 0

def update():
    global current_read, cycle_count
    
    # Pulsing display effect
    display.alpha = 0.5 + (math.sin(time.time() * 8) * 0.2)
    
    write_idx = global_memory.write_index
    
    if current_read != write_idx:
        sample = global_memory.buffer[current_read]
        
        # Optimized Scaling for Visualization
        acc_scale = 0.00008
        gyro_scale = 0.08
        
        # Apply Motion from Shared Memory
        laptop_pivot.rotation_x += sample.gyro_x * gyro_scale
        laptop_pivot.rotation_y += sample.gyro_y * gyro_scale
        laptop_pivot.rotation_z += sample.gyro_z * gyro_scale
        
        laptop_pivot.x += sample.accel_x * acc_scale
        laptop_pivot.y += sample.accel_z * acc_scale
        laptop_pivot.z += sample.accel_y * acc_scale
        
        # Update HUD
        if cycle_count % 10 == 0:
            hud_text.text = f"[ GALACTIC-INS ]\nX_ACCEL: {sample.accel_x}\nY_ACCEL: {sample.accel_y}\nZ_ACCEL: {sample.accel_z}"
        
        # Motion Trail (Glowing Particles)
        if cycle_count % 4 == 0:
            p = Entity(model='cube', color=color.cyan, scale=0.15, position=laptop_pivot.position)
            p.animate_scale(0, duration=1.2)
            p.animate_color(color.clear, duration=1.2)
            destroy(p, delay=1.2)

        current_read = (current_read + 1) % RING_BUFFER_SIZE
        cycle_count += 1

# Cinematic Camera Positioning
camera.position = (15, 10, -25) # Offset angle to see 3D shape
camera.look_at(laptop_pivot)

def input(key):
    if key == 'escape':
        quit()

app.run()
