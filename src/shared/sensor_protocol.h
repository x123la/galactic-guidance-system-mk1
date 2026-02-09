#ifndef SENSOR_PROTOCOL_H
#define SENSOR_PROTOCOL_H

#include <stdint.h>

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
    volatile uint32_t read_index;  // Updated by Ada (Reader)
    Sensor_Sample_t buffer[RING_BUFFER_SIZE];
} Shared_Memory_Area_t;

#endif
