# Galactic Guidance System Mk1 (GGS-Mk1)
## High-Integrity Inertial Navigation System | Technical Manual & System Documentation

**Principal Engineer: Lucas Alonso Basanko**  
**Revision: 2.0.0 (Strict Hardware Edition)**  
**Date: February 2026**

---

## 1. Executive Summary

The **Galactic Guidance System Mk1 (GGS-Mk1)** is a high-integrity Inertial Navigation System (INS) designed for deterministic state estimation. Revision 2.0.0 introduces the **Multi-Process Shared Plane** architecture, allowing multiple independent observers (CLI demos, 3D engines) to attach to a single hardware-near "Writer" core with zero latency. 

This build enforces a **Strict Hardware Policy**: synthetic data is disabled by default to ensure all navigation states are derived from physical motherboard interrupts.

---

## 2. System Architecture

### 2.1 Structural Decomposition
The system is partitioned into four primary functional domains, synchronized via POSIX Shared Memory (`shm_open`):

1.  **Hardware Abstraction Layer (HAL)**: C11 kernel-interconnect. Manages IIO sensor polling and atomic commits.
2.  **Logic and Navigation Core (LNC)**: Ada/SPARK integration kernel. Handles gravity rejection and coordinate transformation.
3.  **Cross-Process Synchronization (CPS)**: Lock-based memory protection (`flock`) and writer-takeover logic.
4.  **Tactical Visualization Engine (TVE)**: 3D real-time telemetry renderer powered by Ursina.

### 2.2 Data Flow & Process Topology
```text
[ PHYSICAL SENSORS ] <--- Linux IIO (Industrial I/O)
       |
       | (400Hz Non-blocking Interrupts)
       v
[ HAL WRITER CORE ] <--- Single Process (Writer Lock: /tmp/ggs_mk1_writer.lock)
       |
       | (Atomic Commit)
       v
[ POSIX SHARED PLANE ] <--- Global Object: /ggs_mk1_shm
       |__________________________________
       |                                  |
       v                                  v
[ LNC OBSERVER ]                   [ TVE VISUALIZER ]
(Formal Math Core)                 (3D Game Engine)
       |                                  |
       v                                  v
[ PROVEN NAV STATE ]               [ TACTICAL HUD ]
```

---

## 3. Advanced Features

### 3.1 Strict Hardware Enforcement
To prevent the propagation of simulated "ghost" data, GGS-Mk1 v2.0.0 will fail-fast if no Linux IIO accelerometer is detected. 
- **Production Mode**: Fails with error if `/sys/bus/iio/devices/iio:device*` is unreachable.
- **Research Mode**: Simulation fallback is only available via the explicit environment override: `GGS_ALLOW_SIM=1`.

### 3.2 Cross-Process Memory Contract
The **Shared Memory Interface (SMI)** utilizes a 64-bit aligned contiguous RAM block.
- **Write head tracking**: Readers track the `write_count` to detect lag.
- **Automatic Resync**: If an observer (e.g., the 3D Engine) falls behind the 1024-slot ring buffer, it automatically resyncs to the live head to maintain real-time fidelity.
- **Writer Takeover**: If the primary writer process stalls for >3 seconds, readers will attempt to promote themselves to Writer status to maintain the telemetry stream.

---

## 4. Mathematical Methodology

### 4.1 State Integration
Acceleration ($a$) is reported in integer `mm/s^2` to maintain precision before being converted to world-frame meters.

1.  **Gravity Rejection**:
    $$a_{linear} = a_{raw} - 9.80665 \, m/s^2$$
2.  **Dt Clamping**:
    To prevent integration explosions during CPU spikes, the time step ($\Delta t$) is strictly clamped:
    $$0.0001s < \Delta t < 0.100s$$

---

## 5. Interface Control Document (ICD)

### 5.1 Shared Memory Layout (64-bit Alignment)

| Offset | Field | Type | Description |
| :--- | :--- | :--- | :--- |
| 0x00 | `write_index` | `uint32_t` | Ring buffer head |
| 0x04 | `read_index` | `uint32_t` | (Legacy) for single-reader compat |
| 0x08 | `write_count` | `uint64_t` | Monotonic sample counter |
| 0x10 | `writer_pid` | `uint32_t` | Active Writer process ID |
| 0x18 | `buffer[1024]`| `Sample_t` | Telemetry array |

---

## 6. Build and Deployment

### 6.1 Unified Build
The system utilizes a central Makefile to coordinate the C-to-Ada bridge.
```bash
make
```

### 6.2 The "Sexy Powers" Manifestation
A global orchestration command is provided for streamlined tactical deployment.
```bash
galacticmanifestyoursexypowersbaby
```

**Operational Sequence:**
1.  **Privilege Check**: Escalates via `sudo` for raw `/dev/` access.
2.  **Core Ignition**: Backgrounds the LNC (Ada Core).
3.  **Visualization Engagement**: Foregrounds the TVE (3D Visualizer).

---

## 7. License

Licensed under the **MIT License**.  
**Copyright (c) 2026 Lucas Alonso Basanko**

---
**DOCUMENTATION END**