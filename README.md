# Galactic Guidance System Mk1 (GGS-Mk1)

Real-time inertial pipeline using:
- `C` sensor driver (`src/c_driver/driver.c`)
- `Ada` navigation core (`src/ada_core/main_ins.adb`)
- `Python` demo and visualizer (`ins_demo.py`, `viz/visualizer.py`)

## What This Build Guarantees

- Uses a real Linux IIO accelerometer when available (`/sys/bus/iio/devices/iio:device*`).
- Will **not silently fake data**.
- Simulation fallback is opt-in only with `GGS_ALLOW_SIM=1`.
- Startup fails fast with clear diagnostics when no sensor stream is available.
- Uses real cross-process shared memory (`shm_open` + `mmap`) for telemetry exchange.
- Enforces a single writer process using a lock file (`/tmp/ggs_mk1_writer.lock`).
- Supports automatic writer takeover: reader processes can promote themselves if the writer disappears.

## Requirements

- Linux
- `gcc`
- `gprbuild` / GNAT toolchain
- `python3`
- Optional visualizer: `python3 -m pip install ursina`

## Build

```bash
make
```

This builds:
- `bin/main_ins` (Ada core + C driver)
- `bin/libdriver.so` (for Python demo/visualizer)

## Run

### Ada navigation core (real sensor mode)

```bash
./bin/main_ins
```

Optional runtime limit:

```bash
./bin/main_ins 10
```

### Python CLI demo

```bash
python3 -u ins_demo.py --seconds 10
```

### Visualizer

```bash
python3 -u viz/visualizer.py
```

## Multi-Process Mode

You can run the producer and one or more readers at the same time:

Terminal 1 (producer):

```bash
GGS_ALLOW_SIM=1 ./bin/main_ins 30
```

Terminal 2 (reader):

```bash
python3 -u ins_demo.py --seconds 10
```

Terminal 3 (reader visualizer):

```bash
python3 -u viz/visualizer.py
```

If a writer already exists, additional processes automatically run in reader-only mode.
If the writer exits, readers attempt one automatic takeover before failing.

## Simulation Fallback (Explicit Only)

If you intentionally want synthetic data:

```bash
GGS_ALLOW_SIM=1 ./bin/main_ins
GGS_ALLOW_SIM=1 python3 -u ins_demo.py
GGS_ALLOW_SIM=1 python3 -u viz/visualizer.py
```

## Status Flags

From `src/shared/sensor_protocol.h`:
- Source:
  - `STATUS_SOURCE_NONE`
  - `STATUS_SOURCE_IIO`
  - `STATUS_SOURCE_SIM`
- Health:
  - `STATUS_FLAG_SENSOR_MISSING`
  - `STATUS_FLAG_SAMPLE_DROPPED`
  - `STATUS_FLAG_DT_CLAMPED`

`STATUS_FLAG_SAMPLE_DROPPED` is reserved for future per-reader loss accounting.

## Shared Memory Contract

The mapped `Shared_Memory_Area_t` header includes:
- `write_index`: ring write head (writer-owned)
- `read_index`: legacy field (not used for multi-reader flow control)
- `write_count`: monotonic sample counter (writer-owned)
- `writer_pid`: active writer PID (0 if none)

Reader behavior:
- Readers attach at the current `write_index` to avoid replaying stale slots.
- Readers do not mutate shared head/tail state.
- If a reader lags by more than ring size, it resyncs to live head and logs a warning.

## Notes

- Acceleration fields are in `mm/s^2` (`9806 ~= 1g`).
- Shared memory object name: `"/ggs_mk1_shm"`.
- `main_ins`, `ins_demo.py`, and `visualizer.py` map the same region across processes.
- Ring behavior is real-time and lossy-by-design under heavy load (latest data wins).
