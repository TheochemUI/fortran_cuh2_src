project('fortcuh2', 'fortran',
  version : '0.0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++20',
                     'debug=true',
                     'buildtype=debug'])

# Add C++ compiler options
_args = [] # Extra arguments
_deps = [] # Dependencies
_linkto = [] # All the sub-libraries
_incdirs = [] # All the includes

add_languages('c', required: true)
cc = meson.get_compiler('c')
# Platform detection
host_system = host_machine.system()
is_windows = host_system == 'windows'
is_mingw = is_windows and cc.get_id() == 'gcc'

# Conditional arguments
if host_system == 'linux'
  _args += '-Wno-return-type'
  _args += '-Wno-switch'
  _args += '-Wno-unused-variable'
  _args += '-Wno-unused-const-variable'
endif

cpu_family = host_machine.cpu_family()

if is_mingw
  # For mingw-w64, don't use LTO
  add_project_arguments('-fno-use-linker-plugin', language: ['c', 'cpp'])
endif

# --------------------- Project
cuh2 = library('fortcuh2',
           'eam_isoc.f90',
           'eamroutines.f90',
           dependencies: _deps,
           cpp_args: _args,
           install : true)
