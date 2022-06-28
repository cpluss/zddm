# Include GTest
include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        release-1.12.0
)
FetchContent_MakeAvailable(googletest)
add_library(GTest::GTest INTERFACE IMPORTED)
target_link_libraries(GTest::GTest INTERFACE gtest_main)

# Add the main ZDDM test
add_executable(zddm_test 
  main.cpp
  ProxyTest.cpp
  TrafficGateTest.cpp
)

target_link_libraries(
  zddm_test
  PRIVATE 
    # Link GTest
    GTest::GTest 
    # Link the main ZDDM library / interface
    zddm
)
add_test(zddm_gtests zddm_test)