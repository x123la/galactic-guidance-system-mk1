from ursina import *
from ursina.prefabs.first_person_controller import FirstPersonController
import ctypes
import os
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
app = Ursina(borderless=False, title="GALACTIC INS VISUALIZER")

# Environment: Deep Void
Entity(model='sphere', scale=1000, texture='sky_default', color=color.black, double_sided=True)

# Lighting: Cinematic Blue/Purple
PointLight(parent=camera, position=(0, 10, -10), color=color.cyan)
AmbientLight(color=color.rgba(10, 10, 30, 255))

# The Tactical Grid (Infinite Pulse)
grid = Entity(model=Grid(100, 100), rotation_x=90, scale=200, color=color.rgba(0, 255, 255, 30))

# The Laptop Avatar: "Cyber-Laptop"
laptop_base = Entity(model='cube', color=color.dark_gray, scale=(4, 0.2, 3))
# Neon Edges
Entity(parent=laptop_base, model='cube', color=color.cyan, scale=(1.02, 0.1, 1.02), mode='wireframe')

# The Screen (Glowing Emissive)
laptop_screen_hinge = Entity(parent=laptop_base, y=0.1, z=1.45)
laptop_screen = Entity(parent=laptop_screen_hinge, model='cube', color=color.black, scale=(4, 2.5, 0.1), origin_y=-0.5)
laptop_screen.rotation_x = -100
# Screen Content (Neon Pulse)
screen_content = Entity(parent=laptop_screen, model='quad', color=color.cyan, scale=(0.9, 0.9), z=-0.6, texture='white_cube')
screen_content.alpha = 0.5

# Data HUD
hud_bg = Entity(parent=camera.ui, model='quad', scale=(0.4, 0.2), position=(-0.65, 0.35), color=color.rgba(0, 0, 0, 150))
hud_text = Text(text="[ SYSTEM ACTIVE ]\nX: 0.0\nY: 0.0\nZ: 0.0", position=(-0.82, 0.43), scale=1.2, color=color.cyan)

# Star Particles
stars = [Entity(model='sphere', color=color.white, scale=random.uniform(0.05, 0.2), 
                position=(random.randint(-100, 100), random.randint(-50, 50), random.randint(-100, 100))) for _ in range(200)]

# Motion Trail
trail = []
TRAIL_MAX = 100

current_read = 0

def update():
    global current_read
    
    # Pulse the screen
    screen_content.alpha = 0.3 + (math.sin(time.time() * 5) * 0.2)
    
    write_idx = global_memory.write_index
    
    if current_read != write_idx:
        sample = global_memory.buffer[current_read]
        
        acc_scale = 0.0001
        gyro_scale = 0.05
        
        # 1. Update Rotation
        laptop_base.rotation_x += sample.gyro_x * gyro_scale
        laptop_base.rotation_y += sample.gyro_y * gyro_scale
        laptop_base.rotation_z += sample.gyro_z * gyro_scale
        
        # 2. Update Position
        laptop_base.x += sample.accel_x * acc_scale
        laptop_base.y += sample.accel_z * acc_scale
        laptop_base.z += sample.accel_y * acc_scale
        
        # 3. Update HUD
        hud_text.text = f"[ GALACTIC INS ]\nX_ACC: {sample.accel_x:d}\nY_ACC: {sample.accel_y:d}\nZ_ACC: {sample.accel_z:d}\nSTATUS: NOMINAL"
        
        # 4. Trail Effect (Neon Particles)
        if cycle_count % 5 == 0:
            p = Entity(model='cube', color=color.cyan, scale=0.1, position=laptop_base.position)
            p.animate_scale(0, duration=1.5, curve=curve.linear)
            p.animate_color(color.clear, duration=1.5)
            destroy(p, delay=1.5)

        current_read = (current_read + 1) % RING_BUFFER_SIZE

# Smooth Camera Follow
camera.position = lerp(camera.position, laptop_base.position + Vec3(0, 15, -25), time.dt * 2)
camera.look_at(laptop_base)

cycle_count = 0
def input(key):
    if key == 'escape':
        quit()

app.run()