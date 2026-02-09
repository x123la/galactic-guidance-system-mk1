# Galactic Guidance System Mk1 (GGS-Mk1)
## 🚀 Formal-Methods Powered Inertial Navigation System

**Developed by: Lucas Alonso Basanko**

---

## 🌌 Overview

The **Galactic Guidance System Mk1** is a high-integrity Inertial Navigation System (INS) engineered to provide precise spatial positioning and orientation. Unlike standard consumer-grade navigation software, GGS-Mk1 is built using a **Zero-Copy Hyper-Plane** architecture, combining the raw hardware efficiency of **C** with the mathematical provability of **Ada (SPARK)**.

This project was conceived as an exercise in "Over-Engineered Genius"—bringing aerospace-grade software reliability to a standard Linux laptop environment.

---

## 🛠 Architectural Philosophy: The "Hyper-Plane"

The system is divided into three strictly isolated domains that interact through a shared memory sanctuary:

### 1. The Hardware Pump (C11)
- **Role**: High-frequency, interrupt-driven data acquisition.
- **Implementation**: Bypasses high-level overhead by communicating directly with the Linux Industrial I/O (IIO) subsystem and HID raw nodes.
- **Efficiency**: Runs as a real-time `SCHED_FIFO` process, pushing raw sensor packets into a lock-free ring buffer.

### 2. The Dimensional Marshal (Ada 2012)
- **Role**: Dimensional safety and physics enforcement.
- **Implementation**: Uses Ada's strict type system to define physical dimensions (`Meters`, `Seconds`, `Acceleration`).
- **Safety**: The compiler physically forbids illegal operations (e.g., adding acceleration to velocity without time integration).

### 3. The Proven Core (SPARK 2014)
- **Role**: The mathematical brain (Kalman Filter / Dead Reckoning).
- **Implementation**: Written in the SPARK subset of Ada.
- **Verification**: Formally proven to be free of runtime errors (No overflows, no division by zero, no out-of-bounds access).

---

## 📐 Mathematical Framework: Dead Reckoning

The GGS-Mk1 implements a continuous integration loop to derive position from noisy acceleration data.

$$Velocity_{t+1} = Velocity_t + (Acceleration_{body} - Gravity) \cdot \Delta t$$
$$Position_{t+1} = Position_t + Velocity_{t+1} \cdot \Delta t$$

### 🛡 Dimensional Protection
In GGS-Mk1, the following code will **fail to compile**, protecting the mission from unit errors:
```ada
-- This results in a COMPILE ERROR
Distance := Speed + Acceleration; 
```

---

## 🛰 Hardware Support & Simulation
The system features a **Hybrid Hardware Driver**:
- **Live Mode**: Attempts to bind to `/dev/HID-SENSOR-*` or `/sys/bus/iio/devices/iio:device0`.
- **Simulation Fallback**: If no hardware is detected, the system engages a high-fidelity **Stochastic Physics Simulator**, modeling sensor noise, bias instability, and random walk.

---

## 📂 Project Structure

```text
.
├── bin/                # Compiled binaries and shared libraries
├── obj/                # Object files and SPARK proof artifacts
├── src/
│   ├── ada_core/       # The Verified Navigation Brain (Ada/SPARK)
│   ├── c_driver/       # The Hardware "Pump" (C)
│   └── shared/         # Memory protocols and shared headers
└── gemini_ins.gpr      # GNAT Project configuration
```

---

## 🚀 Installation & Running

### Prerequisites
- Linux OS
- GNAT Ada Compiler (`sudo apt install gnat gprbuild`)
- GCC

### Build
```bash
gprbuild -P gemini_ins.gpr
```

### Run
```bash
cd bin
sudo ./main_ins
```
*(Sudo is required for raw hardware access via the HID/IIO subsystem)*

---

## 📜 License

**Copyright (c) 2026 Lucas Alonso Basanko**

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.

---

## 🌠 "Because the Universe doesn't use Python."
