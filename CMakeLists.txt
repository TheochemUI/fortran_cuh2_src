cmake_minimum_required(VERSION 3.16)
project(fortcuh2 
    VERSION 0.0.2 
    LANGUAGES Fortran
)

add_library(fortcuh2
    eam_dat.f90
    eamroutines.f90
    eam_isoc.f90
)

target_include_directories(fortcuh2
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<INSTALL_INTERFACE:include>
)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fortcuh2 PRIVATE -Wno-return-type -Wno-unused-variable -fimplicit-none)
endif()

# Build the utility executable for non-subprojects
if(PROJECT_IS_TOP_LEVEL)
    add_executable(tmp_run main.f90)
    target_link_libraries(tmp_run PRIVATE fortcuh2)
endif()

install(TARGETS fortcuh2 EXPORT fortcuh2Targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(EXPORT fortcuh2Targets
    FILE fortcuh2Config.cmake
    DESTINATION lib/cmake/fortcuh2
)
