cmake_minimum_required(VERSION 3.15...4.1)

project(KodiakFS VERSION 1.0
        DESCRIPTION "A distributed file system"
        LANGUAGES CXX C)

find_package(spdlog REQUIRED)
find_package(Boost REQUIRED)

add_subdirectory(metaserver)

