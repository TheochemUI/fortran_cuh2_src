project('fortcuh2', 'fortran',
  version : '0.0.2',
  default_options : ['warning_level=3',
                     'debug=true',
                     'buildtype=debug'])

# Add compiler options
_fargs = [] # Extra arguments
_deps = [] # Dependencies
_linkto = [] # All the sub-libraries
_incdirs = [] # All the includes

# Platform detection
fc = meson.get_compiler('fortran')
host_system = host_machine.system()
is_windows = host_system == 'windows'
is_mingw = is_windows and fc.get_id() == 'gcc'

# Conditional arguments
_fargs += fc.get_supported_arguments([
  '-Wno-return-type',
  '-Wno-unused-variable',
  '-fimplicit-none',
])


# --------------------- Project
cuh2 = library('fortcuh2',
               'eam_dat.f90',
               'eamroutines.f90',
               'eam_isoc.f90',
               dependencies: _deps,
               fortran_args : _fargs,
               install : true)
_linkto += cuh2

fortcuh2_dep = declare_dependency(include_directories: _incdirs,
                                  link_with: _linkto,
                                  dependencies: _deps)

if (not meson.is_subproject())

  cuh2exec = executable('tmp_run', 'main.f90',
                       dependencies: fortcuh2_dep,
                       fortran_args: _fargs)

endif
