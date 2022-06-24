cmake_minimum_required(VERSION 3.9)

project(
  zddm
  LANGUAGES CXX
  VERSION 0.1
  DESCRIPTION "Zero Downtime Data Migration"
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

enable_testing()

add_subdirectory(src)
add_subdirectory(tests)