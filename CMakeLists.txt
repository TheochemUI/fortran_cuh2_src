cmake_minimum_required(VERSION 3.16)
project(fortcuh2
    VERSION 0.1.0
    LANGUAGES Fortran CXX
)

# One place for every .mod, which is also what the package exports.
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

find_package(Threads REQUIRED)

# Vendored neighbour-list backend, built as a single translation unit.
add_library(vesin_internal STATIC vesin/vesin-single-build.cpp)
target_include_directories(vesin_internal PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/vesin>
)
target_link_libraries(vesin_internal PUBLIC Threads::Threads)
set_target_properties(vesin_internal PROPERTIES POSITION_INDEPENDENT_CODE ON)

# vesin Fortran interface, vendored at the same version as vesin/.
add_library(vesin_fortran STATIC
    vesin/fortran/cdef.f90
    vesin/fortran/vesin.f90
)
target_link_libraries(vesin_fortran PUBLIC vesin_internal)
set_target_properties(vesin_fortran PROPERTIES POSITION_INDEPENDENT_CODE ON)

add_library(fortcuh2
    eam_dat.f90
    vesin_cuh2.f90
    eamroutines.f90
    eam_isoc.f90
)

target_link_libraries(fortcuh2 PUBLIC vesin_fortran)

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

install(TARGETS fortcuh2 vesin_fortran vesin_internal EXPORT fortcuh2Targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(EXPORT fortcuh2Targets
    FILE fortcuh2Config.cmake
    DESTINATION lib/cmake/fortcuh2
)
