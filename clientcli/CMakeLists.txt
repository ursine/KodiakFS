cmake_minimum_required(VERSION 3.12)
project(MyFUSEProject CXX C)

find_package(PkgConfig QUIET)
pkg_check_modules(FUSE3 QUIET fuse3)

if(FUSE3_FOUND)
    message(STATUS "Found FUSE 3 (using PkgConfig)")
    # Use FUSE3_INCLUDE_DIRS and FUSE3_LIBRARIES
    #    target_include_directories(YourTarget PRIVATE ${FUSE3_INCLUDE_DIRS})
    #target_link_libraries(YourTarget PRIVATE ${FUSE3_LIBRARIES})
else()
    # Fallback or error handling for FUSE 3
    message(FATAL_ERROR "FUSE 3 not found using PkgConfig")
endif()


