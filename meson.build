project('gambit-build-scm')

gsc_script = find_program('gsc-script')

custom_target('build.c',
    input: '_build.scm',
    output: '_build.c',

    command: [gsc_script, '-:d-', '-debug', '-c', '-o', '@OUTPUT@', '@INPUT@'],

    install_dir: '/lib',
    install: true)

custom_target('build.o1',
    input: '_build.scm',
    output: '_build.o1',

    command: [gsc_script, '-:d-', '-debug', '-o', '@OUTPUT@', '@INPUT@'],

    install_dir: '/lib',
    install: true)
