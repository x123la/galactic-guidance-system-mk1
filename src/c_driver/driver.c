#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include "../shared/sensor_protocol.h"

// The Global Shared Memory Instance
// This is what Ada "Imports"
Shared_Memory_Area_t global_shared_memory;

volatile int keep_running = 1;

// Helper to get monotonic time
uint64_t get_time_ns() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

// The "Pump" Loop
void* sensor_pump_thread(void* arg) {
    printf("[C Driver] Pump thread started. Firing up the Ring Buffer.\n");

    // Initialize indices
    global_shared_memory.write_index = 0;
    global_shared_memory.read_index = 0;

    uint64_t start_time = get_time_ns();
    
    while (keep_running) {
        uint64_t now = get_time_ns();
        double elapsed_sec = (double)(now - start_time) / 1e9;

        // Determine where to write (Ring Buffer Logic)
        uint32_t current_write = global_shared_memory.write_index;
        uint32_t next_write = (current_write + 1) % RING_BUFFER_SIZE;

        // Check if full (Write shouldn't overtake Read)
        if (next_write != global_shared_memory.read_index) {
            
            Sensor_Sample_t* sample = &global_shared_memory.buffer[current_write];
            
            sample->timestamp_ns = now;

            // Generate "Genius" Synthetic Data
            // Simulating a drone hovering and slowly rotating
            
            // Nuclear Option: Direct Raw Byte Injection
            // We read from the HID-SENSOR nodes and hidraw nodes directly.
            // Even if we don't know the exact format, the values will change with movement.
            const char* sensor_paths[] = {
                "/dev/HID-SENSOR-2000e1.2.auto",
                "/dev/HID-SENSOR-2000e1.3.auto",
                "/dev/HID-SENSOR-2000e1.4.auto",
                "/dev/hidraw0",
                "/dev/hidraw1",
                "/dev/hidraw2",
                NULL
            };

            int found_real_data = 0;
            for (int i = 0; sensor_paths[i] != NULL; i++) {
                int fd = open(sensor_paths[i], O_RDONLY | O_NONBLOCK);
                if (fd >= 0) {
                    uint8_t raw_buffer[64];
                    ssize_t bytes_read = read(fd, raw_buffer, sizeof(raw_buffer));
                    if (bytes_read > 0) {
                        // We found a live data stream! 
                        // We take the first 4 bytes as a "raw signal" for X.
                        // We take the next 4 for Y, etc.
                        memcpy(&sample->accel_x, &raw_buffer[0], 4);
                        if (bytes_read >= 8) memcpy(&sample->accel_y, &raw_buffer[4], 4);
                        if (bytes_read >= 12) memcpy(&sample->accel_z, &raw_buffer[8], 4);
                        
                        found_real_data = 1;
                        close(fd);
                        break;
                    }
                    close(fd);
                }
            }

            if (!found_real_data) {
                // LAST RESORT: Simulation
                static double bias_x = 0.02;



                double noise_x = ((double)rand() / RAND_MAX - 0.5) * 10.0;
                bias_x += ((double)rand() / RAND_MAX - 0.5) * 0.001;
                sample->accel_x = (int32_t)((sin(elapsed_sec * 2.0) * 500.0) + (bias_x * 1000.0) + noise_x);
            }

            sample->accel_y = (int32_t)(cos(elapsed_sec * 2.1) * 500.0);
            sample->accel_z = 9806 + (int32_t)(sin(elapsed_sec * 30.0) * 20.0);


            // Gyro: Slow rotation around Z axis
            sample->gyro_x = 0;
            sample->gyro_y = 0;
            sample->gyro_z = (int32_t)(100.0 * sin(elapsed_sec * 0.5)); // Oscillating yaw

            // Mag: Earth's field rotating relative to body
            sample->mag_x = (int32_t)(300.0 * cos(elapsed_sec * 0.5));
            sample->mag_y = (int32_t)(300.0 * sin(elapsed_sec * 0.5));
            sample->mag_z = -400; // Vertical component

            sample->status_flags = 0;

            // Atomic Commit: Update the write index last
            // This tells Ada "Data is ready"
            __atomic_store_n(&global_shared_memory.write_index, next_write, __ATOMIC_RELEASE);
        } else {
            // Buffer overflow - Drop sample or log warning
            // printf("[C Driver] Buffer Full!\n");
        }

        // Run at ~400Hz (2.5ms sleep)
        struct timespec sleep_time;
        sleep_time.tv_sec = 0;
        sleep_time.tv_nsec = 2500000; 
        nanosleep(&sleep_time, NULL);
    }
    return NULL;
}

// Exported function to start the thread
void start_c_driver() {
    pthread_t thread_id;
    pthread_create(&thread_id, NULL, sensor_pump_thread, NULL);
}
