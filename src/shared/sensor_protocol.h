#ifndef SENSOR_PROTOCOL_H
#define SENSOR_PROTOCOL_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Acceleration sample units for accel_x/accel_y/accel_z:
// milli-meters-per-second-squared (mm/s^2), so 1 g ~= 9806.

// Status flags shared by C, Ada, and Python.
// Source flags are mutually exclusive and encoded in the lowest nibble.
#define STATUS_SOURCE_MASK        0x0000000Fu
#define STATUS_SOURCE_NONE        0x00000000u
#define STATUS_SOURCE_IIO         0x00000001u
#define STATUS_SOURCE_SIM         0x00000002u

#define STATUS_FLAG_SENSOR_MISSING 0x00000100u
#define STATUS_FLAG_SAMPLE_DROPPED 0x00000200u
#define STATUS_FLAG_DT_CLAMPED     0x00000400u

#define GGS_SHARED_MEMORY_NAME "/ggs_mk1_shm"

// This struct MUST align perfectly with the Ada record.
// We use fixed-size types (int64_t) to ensure 64-bit alignment.

typedef struct {
    uint64_t timestamp_ns;  // Kernel timestamp (nanoseconds)
    
    // 3-Axis Accelerometer (Raw counts, usually 16-bit, promoted to 32 for safety)
    int32_t accel_x;
    int32_t accel_y;
    int32_t accel_z;
    
    // 3-Axis Gyroscope
    int32_t gyro_x;
    int32_t gyro_y;
    int32_t gyro_z;
    
    // 3-Axis Magnetometer
    int32_t mag_x;
    int32_t mag_y;
    int32_t mag_z;
    
    // Status flags (e.g., Sensor saturation warning)
    uint32_t status_flags;
    
    // Padding to ensure 64-byte cache-line alignment (performance optimization)
    // 8 + (3*4) + (3*4) + (3*4) + 4 = 48 bytes used.
    // We need 16 bytes of padding to reach 64.
    uint8_t padding[16]; 
    
} Sensor_Sample_t;

// The Ring Buffer itself
#define RING_BUFFER_SIZE 1024

typedef struct {
    volatile uint32_t write_index; // Updated by C (Writer)
    volatile uint32_t read_index;  // Legacy field; readers should not mutate in multi-reader mode.
    volatile uint64_t write_count; // Monotonic sample counter (writer-owned).
    volatile uint32_t writer_pid;  // PID of active writer process (0 if none).
    volatile uint32_t reserved0;
    Sensor_Sample_t buffer[RING_BUFFER_SIZE];
} Shared_Memory_Area_t;

// Driver / shared-memory API
void start_c_driver(void);
void stop_c_driver(void);
void* ggs_map_shared_memory(void);
void ggs_unmap_shared_memory(void);

#ifdef __cplusplus
}
#endif

#endif
