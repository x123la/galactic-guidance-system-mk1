#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "../shared/sensor_protocol.h"

static volatile int keep_running = 1;
static volatile int writer_active = 0;
static pthread_t driver_thread;
static pthread_mutex_t driver_lock = PTHREAD_MUTEX_INITIALIZER;
static int shm_fd = -1;
static int writer_lock_fd = -1;
static Shared_Memory_Area_t* shared_memory = NULL;

typedef struct {
    int available;
    char device_name[128];
    char path_x[PATH_MAX];
    char path_y[PATH_MAX];
    char path_z[PATH_MAX];
    double scale;
    int units_inferred;
    int units_in_g;
} iio_accel_source_t;

// Helper to get monotonic time.
static uint64_t get_time_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

static int read_long_from_file(const char* path, long* out_value) {
    FILE* f = fopen(path, "r");
    if (!f) {
        return 0;
    }
    int ok = fscanf(f, "%ld", out_value) == 1;
    fclose(f);
    return ok;
}

static int read_double_from_file(const char* path, double* out_value) {
    FILE* f = fopen(path, "r");
    if (!f) {
        return 0;
    }
    int ok = fscanf(f, "%lf", out_value) == 1;
    fclose(f);
    return ok;
}

static int build_path(char* out, size_t out_size, const char* base, const char* suffix) {
    int written = snprintf(out, out_size, "%s%s", base, suffix);
    return written > 0 && (size_t)written < out_size;
}

static void read_device_name(const char* path, char* out_name, size_t out_len) {
    FILE* f = fopen(path, "r");
    if (!f) {
        snprintf(out_name, out_len, "unknown");
        return;
    }
    if (!fgets(out_name, (int)out_len, f)) {
        snprintf(out_name, out_len, "unknown");
        fclose(f);
        return;
    }
    fclose(f);
    size_t len = strlen(out_name);
    while (len > 0 && (out_name[len - 1] == '\n' || out_name[len - 1] == '\r')) {
        out_name[len - 1] = '\0';
        len--;
    }
}

static int discover_iio_accelerometer(iio_accel_source_t* source) {
    memset(source, 0, sizeof(*source));
    source->scale = 1.0;

    DIR* dir = opendir("/sys/bus/iio/devices");
    if (!dir) {
        return 0;
    }

    struct dirent* entry = NULL;
    while ((entry = readdir(dir)) != NULL) {
        if (strncmp(entry->d_name, "iio:device", 10) != 0) {
            continue;
        }

        char base[PATH_MAX];
        snprintf(base, sizeof(base), "/sys/bus/iio/devices/%s", entry->d_name);

        char path_x[PATH_MAX];
        char path_y[PATH_MAX];
        char path_z[PATH_MAX];
        char path_scale[PATH_MAX];
        char path_name[PATH_MAX];

        if (!build_path(path_x, sizeof(path_x), base, "/in_accel_x_raw") ||
            !build_path(path_y, sizeof(path_y), base, "/in_accel_y_raw") ||
            !build_path(path_z, sizeof(path_z), base, "/in_accel_z_raw") ||
            !build_path(path_scale, sizeof(path_scale), base, "/in_accel_scale") ||
            !build_path(path_name, sizeof(path_name), base, "/name")) {
            continue;
        }

        if (access(path_x, R_OK) != 0 || access(path_y, R_OK) != 0 || access(path_z, R_OK) != 0) {
            continue;
        }

        source->available = 1;
        source->scale = 1.0;
        if (!read_double_from_file(path_scale, &source->scale) || source->scale == 0.0) {
            source->scale = 1.0;
        }

        strncpy(source->path_x, path_x, sizeof(source->path_x) - 1);
        strncpy(source->path_y, path_y, sizeof(source->path_y) - 1);
        strncpy(source->path_z, path_z, sizeof(source->path_z) - 1);
        source->path_x[sizeof(source->path_x) - 1] = '\0';
        source->path_y[sizeof(source->path_y) - 1] = '\0';
        source->path_z[sizeof(source->path_z) - 1] = '\0';
        read_device_name(path_name, source->device_name, sizeof(source->device_name));

        closedir(dir);
        return 1;
    }

    closedir(dir);
    return 0;
}

static int read_iio_accel_sample(iio_accel_source_t* source, int32_t* ax_mmps2, int32_t* ay_mmps2, int32_t* az_mmps2) {
    long raw_x = 0;
    long raw_y = 0;
    long raw_z = 0;
    if (!read_long_from_file(source->path_x, &raw_x) ||
        !read_long_from_file(source->path_y, &raw_y) ||
        !read_long_from_file(source->path_z, &raw_z)) {
        return 0;
    }

    double ax = (double)raw_x * source->scale;
    double ay = (double)raw_y * source->scale;
    double az = (double)raw_z * source->scale;

    if (!source->units_inferred) {
        double norm = sqrt((ax * ax) + (ay * ay) + (az * az));
        // IIO scale is commonly m/s^2/LSB, but some devices report in g.
        // Infer once from the static gravity magnitude near startup.
        source->units_in_g = (norm > 0.5 && norm < 2.5);
        source->units_inferred = 1;
    }

    if (source->units_in_g) {
        ax *= 9.80665;
        ay *= 9.80665;
        az *= 9.80665;
    }

    *ax_mmps2 = (int32_t)llround(ax * 1000.0);
    *ay_mmps2 = (int32_t)llround(ay * 1000.0);
    *az_mmps2 = (int32_t)llround(az * 1000.0);
    return 1;
}

static int allow_simulation_fallback(void) {
    const char* env = getenv("GGS_ALLOW_SIM");
    return (env != NULL && strcmp(env, "1") == 0);
}

static int ensure_shared_memory_mapped(int allow_create) {
    if (shared_memory != NULL) {
        return 1;
    }

    int created = 0;
    if (allow_create) {
        shm_fd = shm_open(GGS_SHARED_MEMORY_NAME, O_RDWR | O_CREAT | O_EXCL, 0660);
        if (shm_fd >= 0) {
            created = 1;
        } else if (errno == EEXIST) {
            shm_fd = shm_open(GGS_SHARED_MEMORY_NAME, O_RDWR | O_CREAT, 0660);
        }
    } else {
        shm_fd = shm_open(GGS_SHARED_MEMORY_NAME, O_RDWR, 0);
    }

    if (shm_fd < 0) {
        return 0;
    }

    if (created) {
        if (ftruncate(shm_fd, (off_t)sizeof(Shared_Memory_Area_t)) != 0) {
            close(shm_fd);
            shm_fd = -1;
            return 0;
        }
    } else {
        struct stat st;
        if (fstat(shm_fd, &st) != 0 || st.st_size < (off_t)sizeof(Shared_Memory_Area_t)) {
            if (ftruncate(shm_fd, (off_t)sizeof(Shared_Memory_Area_t)) != 0) {
                close(shm_fd);
                shm_fd = -1;
                return 0;
            }
        }
    }

    void* mapped = mmap(NULL, sizeof(Shared_Memory_Area_t), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
    if (mapped == MAP_FAILED) {
        close(shm_fd);
        shm_fd = -1;
        return 0;
    }

    shared_memory = (Shared_Memory_Area_t*)mapped;
    if (created) {
        memset(shared_memory, 0, sizeof(*shared_memory));
    }
    return 1;
}

static int acquire_writer_lock(void) {
    writer_lock_fd = open("/tmp/ggs_mk1_writer.lock", O_RDWR | O_CREAT, 0660);
    if (writer_lock_fd < 0) {
        return 0;
    }

    if (flock(writer_lock_fd, LOCK_EX | LOCK_NB) != 0) {
        close(writer_lock_fd);
        writer_lock_fd = -1;
        return 0;
    }
    return 1;
}

static uint64_t next_write_count(void) {
    return __atomic_load_n(&shared_memory->write_count, __ATOMIC_ACQUIRE) + 1ULL;
}

static void populate_simulated_sample(double elapsed_sec, Sensor_Sample_t* sample) {
    static double bias_x = 0.0;
    double noise_x = ((double)rand() / RAND_MAX - 0.5) * 60.0;
    double noise_y = ((double)rand() / RAND_MAX - 0.5) * 60.0;
    double noise_z = ((double)rand() / RAND_MAX - 0.5) * 40.0;

    bias_x += ((double)rand() / RAND_MAX - 0.5) * 1.0;
    if (bias_x > 50.0) {
        bias_x = 50.0;
    } else if (bias_x < -50.0) {
        bias_x = -50.0;
    }

    sample->accel_x = (int32_t)llround((sin(elapsed_sec * 1.6) * 300.0) + bias_x + noise_x);
    sample->accel_y = (int32_t)llround((cos(elapsed_sec * 1.7) * 260.0) + noise_y);
    sample->accel_z = 9806 + (int32_t)llround((sin(elapsed_sec * 4.0) * 120.0) + noise_z);

    sample->gyro_x = (int32_t)llround(20.0 * sin(elapsed_sec * 0.5));
    sample->gyro_y = (int32_t)llround(30.0 * cos(elapsed_sec * 0.4));
    sample->gyro_z = (int32_t)llround(50.0 * sin(elapsed_sec * 0.6));

    sample->mag_x = (int32_t)llround(300.0 * cos(elapsed_sec * 0.3));
    sample->mag_y = (int32_t)llround(300.0 * sin(elapsed_sec * 0.3));
    sample->mag_z = -400;
}

static void* sensor_pump_thread(void* arg) {
    (void)arg;
    printf("[C Driver] Pump thread started.\n");

    writer_active = 1;
    memset(shared_memory, 0, sizeof(*shared_memory));
    __atomic_store_n(&shared_memory->writer_pid, (uint32_t)getpid(), __ATOMIC_RELEASE);
    srand((unsigned int)get_time_ns());

    iio_accel_source_t accel_source;
    int have_iio = discover_iio_accelerometer(&accel_source);
    int use_sim = 0;
    uint32_t base_status = STATUS_SOURCE_NONE;

    if (have_iio) {
        base_status = STATUS_SOURCE_IIO;
        printf("[C Driver] Using real IIO accelerometer: %s\n", accel_source.device_name);
    } else if (allow_simulation_fallback()) {
        use_sim = 1;
        base_status = STATUS_SOURCE_SIM;
        printf("[C Driver] No IIO accelerometer found. Falling back to simulation (GGS_ALLOW_SIM=1).\n");
    } else {
        base_status = STATUS_SOURCE_NONE | STATUS_FLAG_SENSOR_MISSING;
        printf("[C Driver] ERROR: No real IIO accelerometer detected.\n");
        printf("[C Driver] Set GGS_ALLOW_SIM=1 only if you explicitly want simulation fallback.\n");
    }

    uint64_t start_time = get_time_ns();

    while (keep_running) {
        uint64_t now = get_time_ns();
        double elapsed_sec = (double)(now - start_time) / 1e9;

        uint32_t current_write = __atomic_load_n(&shared_memory->write_index, __ATOMIC_ACQUIRE);
        uint32_t next_write = (current_write + 1) % RING_BUFFER_SIZE;

        Sensor_Sample_t* sample = &shared_memory->buffer[current_write];
        memset(sample, 0, sizeof(*sample));
        sample->timestamp_ns = now;
        sample->status_flags = base_status;

        if (have_iio) {
            int ok = read_iio_accel_sample(&accel_source, &sample->accel_x, &sample->accel_y, &sample->accel_z);
            if (!ok) {
                sample->status_flags = (base_status | STATUS_FLAG_SENSOR_MISSING);
            }
        } else if (use_sim) {
            populate_simulated_sample(elapsed_sec, sample);
        } else {
            // No source available: publish heartbeat samples with missing-sensor status.
            sample->status_flags = STATUS_SOURCE_NONE | STATUS_FLAG_SENSOR_MISSING;
        }

        __atomic_store_n(&shared_memory->write_count, next_write_count(), __ATOMIC_RELEASE);
        __atomic_store_n(&shared_memory->write_index, next_write, __ATOMIC_RELEASE);

        struct timespec sleep_time;
        if (have_iio) {
            sleep_time.tv_sec = 0;
            sleep_time.tv_nsec = 5000000;   // ~200 Hz
        } else if (use_sim) {
            sleep_time.tv_sec = 0;
            sleep_time.tv_nsec = 2500000;   // ~400 Hz
        } else {
            sleep_time.tv_sec = 0;
            sleep_time.tv_nsec = 100000000; // 10 Hz heartbeat
        }
        nanosleep(&sleep_time, NULL);
    }

    writer_active = 0;
    __atomic_store_n(&shared_memory->writer_pid, 0U, __ATOMIC_RELEASE);
    if (writer_lock_fd >= 0) {
        flock(writer_lock_fd, LOCK_UN);
        close(writer_lock_fd);
        writer_lock_fd = -1;
    }
    return NULL;
}

void start_c_driver(void) {
    pthread_mutex_lock(&driver_lock);

    if (!ensure_shared_memory_mapped(1)) {
        perror("[C Driver] shared memory init failed");
        pthread_mutex_unlock(&driver_lock);
        return;
    }

    if (writer_active || writer_lock_fd >= 0) {
        pthread_mutex_unlock(&driver_lock);
        return;
    }

    if (!acquire_writer_lock()) {
        printf("[C Driver] Writer lock is held by another process. Running as reader only.\n");
        pthread_mutex_unlock(&driver_lock);
        return;
    }

    keep_running = 1;
    if (pthread_create(&driver_thread, NULL, sensor_pump_thread, NULL) != 0) {
        perror("[C Driver] pthread_create failed");
        flock(writer_lock_fd, LOCK_UN);
        close(writer_lock_fd);
        writer_lock_fd = -1;
        pthread_mutex_unlock(&driver_lock);
        return;
    }
    pthread_detach(driver_thread);
    pthread_mutex_unlock(&driver_lock);
}

void stop_c_driver(void) {
    keep_running = 0;
}

void* ggs_map_shared_memory(void) {
    pthread_mutex_lock(&driver_lock);
    int ok = ensure_shared_memory_mapped(1);
    void* result = ok ? (void*)shared_memory : NULL;
    pthread_mutex_unlock(&driver_lock);
    return result;
}

void ggs_unmap_shared_memory(void) {
    pthread_mutex_lock(&driver_lock);
    if (shared_memory != NULL) {
        munmap((void*)shared_memory, sizeof(Shared_Memory_Area_t));
        shared_memory = NULL;
    }
    if (shm_fd >= 0) {
        close(shm_fd);
        shm_fd = -1;
    }
    pthread_mutex_unlock(&driver_lock);
}
