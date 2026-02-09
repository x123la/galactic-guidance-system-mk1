# Galactic Guidance System Mk1 (GGS-Mk1)
## High-Integrity Inertial Navigation System

**Author: Lucas Alonso Basanko**

---

## Technical Overview

GGS-Mk1 is a high-integrity Inertial Navigation System (INS) designed for precise spatial tracking using a multi-language, zero-copy architecture. The system employs **Ada/SPARK** for its mathematical core to leverage formal verification and **C** for its hardware abstraction layer to ensure low-latency data acquisition.

The primary objective of the system is to provide a reliable Dead Reckoning solution by fusing high-frequency sensor data within a strictly typed physical framework.

## Architecture: The Zero-Copy Hyper-Plane

The system is structured into three isolated domains that communicate through a shared, memory-mapped sanctuary to eliminate data duplication overhead.

### 1. Hardware Abstraction Layer (C11)
- **Responsibility**: Real-time data acquisition from the Linux Industrial I/O (IIO) and HID subsystems.
- **Execution**: Runs as a high-priority thread utilizing raw Linux syscalls to minimize jitter.
- **Data Flow**: Populates a lock-free ring buffer in a memory region shared with the logic core.

### 2. Physical Type System (Ada 2012)
- **Responsibility**: Enforcement of dimensional analysis and physical laws.
- **Implementation**: Utilizes Ada's strong typing to define unique dimensions for `Acceleration`, `Velocity`, and `Position`.
- **Safety**: Semantic errors, such as the improper mixing of units, are caught at compile-time rather than at runtime.

### 3. Navigation Core (SPARK 2014)
- **Responsibility**: State estimation and integration logic.
- **Verification**: The core is written in the SPARK subset of Ada, allowing for formal proof of the absence of runtime errors (AoRTE), including overflows and division by zero.
- **Algorithm**: Implements a discrete integration loop with gravity compensation and noise filtering.

## Mathematical Implementation

GGS-Mk1 performs state estimation through the following integration process:

1. **Gravity Rejection**:
   $$a_{world} = R(q) \cdot a_{body} - g$$
2. **First Integration (Velocity Update)**:
   $$v_{t+1} = v_t + a_{world} \cdot \Delta t$$
3. **Second Integration (Position Update)**:
   $$p_{t+1} = p_t + v_{t+1} \cdot \Delta t$$

## Software Structure

```text
.
├── src/
│   ├── ada_core/       # Formal logic and navigation core (SPARK)
│   ├── c_driver/       # Hardware-near data pump (C)
│   └── shared/         # Shared memory protocols and structures
├── gemini_ins.gpr      # GPRbuild project file
└── bin/                # Executable binaries
```

## Build and Execution

### System Requirements
- Linux Environment
- GNAT Ada Toolchain (2012+)
- GPRbuild
- GCC

### Compilation
```bash
gprbuild -P gemini_ins.gpr
```

### Execution
The binary requires elevated privileges to access raw hardware character devices:
```bash
sudo ./bin/main_ins
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

Copyright (c) 2026 Lucas Alonso Basanko