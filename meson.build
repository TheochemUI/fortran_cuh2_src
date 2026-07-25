project(
    'fortcuh2',
    ['fortran', 'cpp'],
    version: '0.1.0',
    default_options: ['warning_level=3', 'debug=true', 'buildtype=debug'],
)

# Add compiler options
_fargs = []  # Extra arguments
_deps = []  # Dependencies
_linkto = []  # All the sub-libraries
_incdirs = []  # All the includes

# Platform detection
fc = meson.get_compiler('fortran')
cppc = meson.get_compiler('cpp')
host_system = host_machine.system()
is_windows = host_system == 'windows'
is_mingw = is_windows and fc.get_id() == 'gcc'

# Conditional arguments
_fargs += fc.get_supported_arguments(
    [
        '-Wno-return-type',
        '-Wno-unused-variable',
        '-fimplicit-none',
    ],
)

# --------------------- vesin
# Vendored neighbour-list backend, built as a single translation unit.
# vesin threads its builds with std::thread only, so no OpenMP here.
vesin_int_deps = [dependency('threads')]
vesin_int = static_library(
    'vesin_internal',
    ['vesin/vesin-single-build.cpp'],
    dependencies: vesin_int_deps,
    # Consumers link statically; hiding the symbols keeps them out of the
    # enclosing shared objects' dynamic tables.
    cpp_args: cppc.get_supported_arguments(['-fvisibility=hidden']),
    pic: true,
)
vesin_dep = declare_dependency(
    link_with: vesin_int,
    dependencies: vesin_int_deps,
    include_directories: include_directories('vesin'),
)

# vesin Fortran interface, vendored at the same version as vesin/. The CuH2
# pair-list helper consumes `use vesin` through vesin_f_dep.
vesin_fortran_lib = static_library(
    'vesin_fortran',
    [
        'vesin/fortran/cdef.f90',
        'vesin/fortran/vesin.f90',
    ],
    fortran_args: _fargs,
    dependencies: vesin_dep,
    pic: true,
)
vesin_f_dep = declare_dependency(
    link_with: vesin_fortran_lib,
    dependencies: vesin_dep,
    include_directories: vesin_fortran_lib.private_dir_include(),
)
_deps += [vesin_f_dep]

# --------------------- Project
cuh2 = library(
    'fortcuh2',
    'eam_dat.f90',
    'vesin_cuh2.f90',
    'eamroutines.f90',
    'eam_isoc.f90',
    dependencies: _deps,
    fortran_args: _fargs,
    install: true,
)
_linkto += cuh2

fortcuh2_dep = declare_dependency(
    include_directories: _incdirs,
    link_with: _linkto,
    dependencies: _deps,
)

if (not meson.is_subproject())

    cuh2exec = executable(
        'tmp_run',
        'main.f90',
        dependencies: fortcuh2_dep,
        fortran_args: _fargs,
    )

endif
