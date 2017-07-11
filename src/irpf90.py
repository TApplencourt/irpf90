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
        # Create a dot reprenstion of the dependency graph.
        # Merge inside a subgraph the Entity provided together

        def print_full_diagram(l_entity):

            l_entity_not_leaf = [e for e in l_entity if e.needs]
            print 'digraph Full { '
            for e in l_entity_not_leaf:
                print '   %s -> { %s } ' % (e.name, ' '.join(e.needs))
            print '}'

        def print_subgraph(l_tuple, name, color):
            for i, s in enumerate(l_tuple):
                print '   subgraph cluster_%s_%s {' % (name, i)
                print '       %s ' % ' '.join(s)
                print '       color = %s ' % color
                print '   }'

        comm_world.t_filename_parsed_text  # Initialize entity need. Dirty I know.

        print_full_diagram(comm_world.d_entity.values())

        print 'digraph Compact { '
        print '   graph [ordering="out" splines=true overlap=false];'

        l_main_usr = set([entity for entity in comm_world.d_entity.values() if entity.is_main])
        l_main_head_usr = set([entity for entity in l_main_usr if entity.l_others_name])
        l_set_main_head_name = [set(e.l_name) for e in l_main_head_usr]

        print_subgraph(l_set_main_head_name, 'usr', color='blue')

        from util import l_dummy_entity

        l_set_dummy_name = l_dummy_entity(comm_world.d_entity)
        print_subgraph(l_set_dummy_name, 'dummy', color='red')

        #~=~=~=~=
        # Create List Node Uniq
        #~=~=~=~=

        from util import split_l_set, flatten
        l_main_dummy_name, s_exculde_dummy_name = split_l_set(l_set_dummy_name)
        l_name_dummy_name_flatten = flatten(l_set_dummy_name)

        l_main_head_dummy = set([comm_world.d_entity[name] for name in l_name_dummy_name_flatten])
        s_exculde_dummy = set([comm_world.d_entity[name] for name in s_exculde_dummy_name])

        l_node_uniq = (l_main_usr | l_main_head_dummy) - s_exculde_dummy

        #~=~=~=~=
        # Create All edge
        #~=~=~=~=
        # We need to remove the spurious edge caused by the the dummy multiples providers 
        d_need = dict()
        for e in l_node_uniq:
            d_need[e.name] = set(e.needs)

    #~=~=~=~=
    # Create All edge
    #~=~=~=~=
    # Draw the eddge
    # If a arrow if arriving into Multipliple provider and if it is bold this mean it use all the entity inside it.

        from util import uniquify
        l_set_multiple = uniquify(l_set_dummy_name + l_set_main_head_name)

        l_name_usr = [e.name for e in l_main_head_usr]
        for source, l_target in d_need.items():

            if source in l_name_usr:
                color = 'blue'
            elif source in l_name_dummy_name_flatten:
                color = 'red'
            else:
                color = 'black'

            for s in l_set_multiple:
                if s.issubset(l_target):
                    print ' %s -> %s [color="%s", penwidth=2]' % (source, sorted(s).pop(), color)
                    l_target = l_target - s

            if l_target:
                print ' %s -> { %s } [color="%s"]' % (source, '  '.join(l_target), color)

        print '   }'
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

    import irp_stack
    irp_stack.create()

    comm_world.create_buildfile(command_line.do_ninja)
    comm_world.write_modules()

    comm_world.create_touches()
    comm_world.create_man()

    if command_line.do_debug or command_line.do_assert:
        comm_world.create_stack()

    if command_line.do_profile:
        import profile
        profile.run(comm_world.d_entity)

    if command_line.do_openmp:
        comm_world.create_lock()


if __name__ == '__main__':
    main()
