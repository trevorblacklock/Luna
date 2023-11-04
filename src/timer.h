# pragma once

# include <chrono>

class Timer {
public:
void start() {start_time = std::chrono::high_resolution_clock::now();};
void stop() {end_time = std::chrono::high_resolution_clock::now();};
uint64_t elapsed() const {return std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();};
private:
std::chrono::high_resolution_clock::time_point start_time;
std::chrono::high_resolution_clock::time_point end_time;
};
