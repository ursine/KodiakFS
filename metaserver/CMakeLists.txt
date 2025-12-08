add_executable(metaserver
        main.cpp sqlite3.c sqlite3.h sqlite3ext.h
        sqlhandler.cpp
        sqlhandler.h)

target_link_libraries(metaserver PRIVATE spdlog::spdlog)