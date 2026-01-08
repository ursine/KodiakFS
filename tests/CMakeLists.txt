cmake_minimum_required(VERSION 3.20)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")


find_package(Erlang REQUIRED)

project(learning LANGUAGES C CXX)

add_executable(learning main.cpp)
target_include_directories(learning PRIVATE Erlang::EI)
target_link_libraries(learning PRIVATE Erlang::EI)
