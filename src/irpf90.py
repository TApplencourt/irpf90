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

	comm_world.t_filename_parsed_text # Initialize entity need. Dirty I know.

	from util import l_dummy_entity, split_l_set
	#print len(comm_world.d_entity)
	#print sum(len(i.needs) for i in comm_world.d_entity.values())
	#print l_dummy_entity(comm_world.d_entity)
	#print len(l_dummy_entity(comm_world.d_entity))
	#print sum(len(i) for i in l_dummy_entity(comm_world.d_entity))


	l_main_usr =  set([entity for entity in comm_world.d_entity.values() if entity.is_main])
        l_main_head_usr = set([entity for entity in l_main_usr if  entity.others_entity_name])
	l_main_atomic_usr = l_main_usr - l_main_head_usr

	print 'digraph Full { '
	for e in comm_world.d_entity.values():
		if e.needs: 
			print '   %s -> { %s } ' % (e.name, ' '.join(e.needs))
	
	print '}'
	print ''

	print 'digraph Small { '
	print '   graph [ordering="out"];'
        for e in l_main_head_usr:
                print '   subgraph cluster%s {' % e.name
                print '       %s ' % ' '.join([e.name] + e.others_entity_name)
                print '   }'


	
        l_set_dummy_name= l_dummy_entity(comm_world.d_entity)
        for i,s in enumerate(l_set_dummy_name):
                print '   subgraph cluster%s {' % i
                print '       %s ' % ' '.join(s)
                print '      color = blue'
                print '   }'


	# We do exactly like the multi-provider.
	l_main_dummy_name, s_exculde_dummy_name = split_l_set(l_set_dummy_name)
	from util import flatten
	l_dummy_name = flatten(l_set_dummy_name)
	l_main_head_dummy = [comm_world.d_entity[name] for name in l_main_dummy_name]


        # Optimisation
	# 1) We merge the depency of multiple-provider. All entity into a multiple provider are the same.
        # 2) For the automatic one, we draw only the arrow for one parent.

	for e in (e for e in l_main_atomic_usr if e.needs and e.name not in l_dummy_name):
                needs_filter = set(e.needs) - s_exculde_dummy_name
                if set(e.needs) != needs_filter:
                        needs_filter = set(e.needs) - s_exculde_dummy_name
                        for s in needs_filter:
                                if s in l_dummy_name:
                                        print '   %s -> { %s } [color=blue, penwidth=2]' % (e.name, s)

                                else:
                                        print '   %s -> { %s }' % (e.name, s)
                else:
                        print '   %s -> { %s }' % (e.name, ' ; '.join(e.needs))

	for e in (e for e in l_main_head_usr if e.needs and e.name not in l_dummy_name):
		needs_filter = set(e.needs) - s_exculde_dummy_name
                if set(e.needs) != needs_filter:
                        needs_filter = set(e.needs) - s_exculde_dummy_name
			for s in needs_filter:
				if s in l_dummy_name:				
                      			print '   %s -> { %s } [color=blue, penwidth=2]' % (e.name, s)

				else:
					print '   %s -> { %s } [penwidth=2]' % (e.name, s)
                else:
                        print '   %s -> { %s } [penwidth=2]' % (e.name, ' ; '.join(e.needs))

        for e in (e for e in l_main_head_dummy if e.needs):
                print '   %s -> { %s } [color=blue, penwidth=2]' % (e.name, ' ; '.join(e.needs))

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
