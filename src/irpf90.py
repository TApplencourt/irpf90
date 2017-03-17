#!/usr/bin/env python
#   IRPF90 is a Fortran90 preprocessor written in Python for programming using
#   the Implicit Reference to Parameters (IRP) method.
#   Copyright (C) 2009 Anthony SCEMAMA 
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#   Anthony Scemama
#   LCPQ - IRSAMC - CNRS
#   Universite Paul Sabatier
#   118, route de Narbonne      
#   31062 Toulouse Cedex 4      
#   scemama@irsamc.ups-tlse.fr

import vim
import os, sys
try:
    wd = os.path.abspath(os.path.dirname(__file__))
    sys.path.insert(0, (wd + "/../src/"))
except:
    pass

from command_line import command_line
from irpy_files import Irpy_comm_world

def main():

    vim.install()

    if command_line.do_help:
        command_line.usage()
        return 
    if command_line.do_version:
        from version import version
        print version
        return 

    if command_line.do_init:
        from build_file import create_generalmakefile
        create_generalmakefile(command_line.do_ninja)
        return

    comm_world = Irpy_comm_world()

    if command_line.do_graph:
        comm_world.t_filename_parsed_text # Initialize entity need. Dirty I know.

        print 'graph { '
        for name,entity in comm_world.d_entity.items():
                if entity.needs:
                        print '   {0} -> {1}'.format(name, ' '.join(entity.needs))
        print '}'
        return


    if command_line.do_preprocess:
        for filename, text in comm_world.preprocessed_text:
          if filename in command_line.preprocessed:
             for line in text:
                 print line.text
        return

    if command_line.do_touch:
        for var in command_line.touched:
            if var not in comm_world.d_entity:
                print "%s is not an IRP entity" % var
            else:
                print "Touching %s invalidates the following entities:" % var
                for x in sorted(d_entity[var].parents):
                    print "- %s" % (x, )
        return

    if command_line.do_codelet:
        import profile
        profile.build_rdtsc()
        import codelet
        codelet.run()

    if not command_line.do_run:
        return

    comm_world.create_buildfile(command_line.do_ninja)
    comm_world.write_modules()
   
    comm_world.create_touches()
    comm_world.create_man()

    if command_line.do_profile:
        import profile
        profile.run(comm_world.d_entity)

    if command_line.do_openmp:
        comm_world.create_lock()

if __name__ == '__main__':
    main()
