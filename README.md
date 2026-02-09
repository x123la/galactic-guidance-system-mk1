# Galactic Guidance System Mk1 (GGS-Mk1)
## High-Integrity Inertial Navigation System | Technical Manual & System Documentation

**Principal Engineer: Lucas Alonso Basanko**
**Revision: 1.0.0**
**Date: February 2026**

---

## 1. Executive Summary

The **Galactic Guidance System Mk1 (GGS-Mk1)** is a specialized Inertial Navigation System (INS) designed for real-time state estimation in high-criticality environments. The system architecture is predicated on the **Zero-Copy Hyper-Plane** model, ensuring deterministic data flow between a hardware-near C11 acquisition layer and a formally verified Ada/SPARK 2014 logic core. 

The GGS-Mk1 provides a foundation for Pedestrian Dead Reckoning (PDR) and Vehicle Dead Reckoning (VDR) by utilizing high-fidelity sensor fusion and double-integration of acceleration vectors within a strictly partitioned memory space.

---

## 2. System Architecture

### 2.1 Structural Decomposition
The system is partitioned into three distinct functional domains to ensure maximum isolation and fault containment:

1.  **Hardware Abstraction Layer (HAL)**: Low-level C driver interface.
2.  **Logic and Navigation Core (LNC)**: Formally verified Ada/SPARK kernel.
3.  **Shared Memory Interface (SMI)**: The synchronization plane.

### 2.2 Data Flow Map (ASCII)
```text
[ PHYSICAL HARDWARE ]
       |
       | (Interrupt / Syscall)
       v
[ C11 HARDWARE PUMP ]  <--- Managed by HAL
       |
       | (Lock-Free Write)
       v
[ SHARED MEMORY RING BUFFER ] <--- SMI (Shared Memory Interface)
       |
       | (Type-Safe Overlay)
       v
[ ADA/SPARK NAV CORE ] <--- Managed by LNC
       |
       | (Double Integration)
       v
[ TELEMETRY / OUTPUT ]
```

---

## 3. Detailed Component Specifications

### 3.1 Hardware Abstraction Layer (HAL)
The HAL is implemented in C11 to provide optimal access to the Linux kernel's `iio` and `hidraw` subsystems. 
- **Non-Blocking I/O**: The driver utilizes `O_NONBLOCK` flag on device character nodes to prevent thread starvation during sensor wait states.
- **Atomic Commits**: Data availability is signaled to the Ada core via an `__atomic_store_n` operation on the `write_index`, ensuring the logic core never reads a partial or "torn" sample.

### 3.2 The Physics Engine & Type System
One of the core innovations of the GGS-Mk1 is its **Dimensional Safety Layer**. By leveraging Ada's user-defined scalar types, we create a compiler-enforced boundary between different physical dimensions.

**Example of Type Enforcement:**
```ada
type Meters is new Float_64;
type Seconds is new Float_64;
type Velocity is new Float_64;

-- The following will generate a compile-time error:
Result := Velocity_Value + Distance_Value; 
-- Correct implementation:
Result := Velocity_Value + (Acceleration * Delta_T);
```

### 3.3 Formal Verification (SPARK)
The Navigation Core is restricted to the **SPARK subset of Ada**. This allows the use of automated theorem provers to verify:
- **Absence of Runtime Errors (AoRTE)**: Mathematical proof that the system cannot crash due to overflows, underflows, or division by zero.
- **Contract-Based Programming**: Every subprogram is defined with `Pre` and `Post` conditions, ensuring that the mathematical state remains within valid physical bounds at all times.

---

## 4. Mathematical Methodology

### 4.1 State Integration
The GGS-Mk1 utilizes a discrete-time integration approach for deriving position from acceleration.

1.  **Gravity Compensation**:
    The system identifies the gravity vector ($g \approx 9.80665 \text{ m/s}^2$) and removes its influence from the body-frame acceleration to isolate linear movement.
2.  **Numerical Integration**:
    The system employs the trapezoidal rule for updating state vectors:
    - $\Delta v = a \cdot \Delta t$
    - $\Delta p = v \cdot \Delta t$

### 4.2 Error Modeling
To ensure the system remains robust even when physical hardware is unavailable, the GGS-Mk1 includes a **Stochastic Error Model** in its fallback driver:
- **Bias Instability**: Modeled as a Random Walk.
- **Gaussian Noise**: Modeled with a configurable standard deviation.

---

## 5. Interface Control Document (ICD)

### 5.1 Shared Memory Layout
The Shared Memory Interface is a contiguous block of RAM partitioned as follows:

| Offset | Field | Type | Description |
| :--- | :--- | :--- | :--- |
| 0x00 | `write_index` | `uint32_t` | Latest sample index (updated by C) |
| 0x04 | `read_index` | `uint32_t` | Last processed index (updated by Ada) |
| 0x08 | `buffer[]` | `Sensor_Sample_t` | Ring buffer of 1024 samples |

### 5.2 Sensor Sample Structure (64-byte Aligned)
- `timestamp_ns`: 64-bit unsigned integer.
- `accel_x/y/z`: 32-bit signed integers (milli-g scale).
- `gyro_x/y/z`: 32-bit signed integers.
- `status_flags`: 32-bit bitfield.
- `padding`: 16-byte alignment buffer.

---

## 6. Build and Maintenance

### 6.1 Toolchain Requirements
- **GNAT-12+**: For Ada/SPARK compilation.
- **GPRBuild**: Project orchestration.
- **GCC**: C driver compilation.

### 6.2 Calibration Procedure
The system requires an initial stationary period (3-5 seconds) upon startup to calibrate the accelerometer bias. During this phase, the `Main_INS` task calculates the zero-g offset for all axes.

---

## 7. License and Copyright

This documentation and the associated source code are licensed under the **MIT License**.

**Copyright (c) 2026 Lucas Alonso Basanko**

---
**END OF DOCUMENTATION**
