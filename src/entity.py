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

from irpf90_t import *
from util import *
from command_line import command_line
import sys
from lib.manager import irpy


class Entity(object):
    '''All lines between BEGIN_PROVIDER and END_PROVIDER included

    Note: Those lines can define multiple Provider, one need to pick one.
    '''

    ############################################################
    def __init__(self, text, label, name=None, comm_world=None):
        # (list[str], str, int, Irpy_comm_world)
        '''Instantiate the object. 

	Args:
		text: List of lines between BEGIN_PROVIDER and END_PROVIDER included
		int:  An unique int id (usefull when profilling)
		name: The name of the provider defined after the chosen BEGIN_PROVIDER statement
		comm_world: A object to communicate we the external world.
	'''

        assert type(text) == list
        assert len(text) > 0
        assert type(text[0]) == Begin_provider

        self.label = label
        self.text = text

        self.same_as = text[0].filename[1]
        self.name = name if name else self.same_as

        self.comm_world = comm_world

    # ~ # ~ # ~
    # G l o b a l   P r o p e r t y
    # ~ # ~ # ~
    @irpy.lazy_property
    def d_entity(self):
        # () -> Dict[str,Entity]
        '''Create an alias to the global dictionary of Entity.

	Note: Be aware of the possiblity of Cyclic Dependency.
	'''
        return self.comm_world.d_entity

    @irpy.lazy_property
    def cm_t_filename_parsed_text(self):
        # () -> Tuple[str, Parsed_text]
        '''Create an alias to the global tuple for parsed text

	Note: self.comm_world.t_filename_parsed_text need d_entity. 
		Be aware of the possibility of Cyclic Dependency
	'''
        return self.comm_world.t_filename_parsed_text

    @irpy.lazy_property
    def d_type_lines(self):
        # () -> Dict[Line, Tuple[int,Line] ]
        '''Contruct a mapping table between the type of the line and the possition'''
        from collections import defaultdict
        d = defaultdict(list)
        for i, line in enumerate(self.text):
            d[type(line)] += [(i, line)]
        return d

    # ~ # ~ # ~
    # M u l t i p l e   P r o v i d e r   H a n d l e r 
    # ~ # ~ # ~
    @irpy.lazy_property
    def is_main(self):
        # () -> bool
        '''Check if this Entity is the main one
	
	Exemple:
		BEGIN_PROVIDER [pi, double precision] &
                BEGIN_PROVIDER [e, double preision]
	
		return True for 'pi' and False for 'e'
	'''
        return self.name == self.same_as

    @irpy.lazy_property
    def prototype(self):
        # () -> Line
        '''Find the declaration statement associated with the name of the provider

	Exemple:
		BEGIN_PROVIDER [pi, double precision] &
		BEGIN_PROVIDER [e, double preision]

		if self.name == e, will return BEGIN_PROVIDER [e, double preision]
	'''

        d = self.d_type_lines
        return next(line for _, line in d[Begin_provider] + d[Cont_provider]
                    if line.filename[1] == self.name)

    @irpy.lazy_property
    def l_name(self):
        # () -> List[str]
        d = self.d_type_lines
        return [line.filename[1] for _, line in d[Begin_provider] + d[Cont_provider]]

    @irpy.lazy_property
    def l_others_name(self):
        # () -> List[str]
        '''Extract the other entity-name defined'''
        return [name for name in self.l_name if not name == self.name]

    @irpy.lazy_property
    def doc(self):
        # () -> List[str]
        doc = [line.text.lstrip()[1:] for _, line in self.d_type_lines[Doc]]
        if not doc:
            logger.warning("Entity '%s' is not documented" % (self.name))
        return doc

    @irpy.lazy_property
    def documented(self):
        #() -> bool
        return bool(self.doc)

    # ~ # ~ # ~
    # T o u c h / R E A D / W R O T E 
    # ~ # ~ # ~

    @irpy.lazy_property_mutable
    def is_written(self):
        #() -> bool
        '''Check if it will be written on disk'''
        return any(self.d_entity[i].is_written for i in self.parents)

    @irpy.lazy_property
    def io_er(self):
        if not self.is_main:
            result = []

        from util import mangled
        from util import ashes_env
        name = self.name

        d_template = {
            'name': name,
            'fmodule': self.fmodule,
            'same_as': self.same_as,
            'do_debug': command_line.do_debug,
            'children': mangled(self.needs, self.d_entity),
            'group_entity': [{
                'name': n,
                'dim': build_dim(
                    self.d_entity[n].dim, colons=True)
            } for n in self.l_name]
        }

        return ashes_env.render('ioer.f90', d_template).split('!TOKEN_SPLIT')

    @irpy.lazy_property
    def reader(self):
        return self.io_er[1].split('\n')

    @irpy.lazy_property
    def writer(self):
        return self.io_er[0].split('\n')

    @irpy.lazy_property_mutable
    def is_read(self):
        '''Check if it  will be read from disk'''
        return any(self.d_entity[i].is_read for i in self.parents)

    @irpy.lazy_property
    def is_source_touch(self):
        return (Touch in self.d_type_lines or SoftTouch in self.d_type_lines)

    @irpy.lazy_property_mutable
    def is_self_touched(self):
        '''Cehck if it will be modified (touch)'''
        return False

    @irpy.lazy_property
    def is_touched(self):
        '''If any of the children is touched, the entity is touched'''
        if self.is_self_touched or any(self.d_entity[i].is_touched for i in self.children):
            return True

        return False

    # ~ # ~ # ~
    # INCLUDE, USE, CALL
    # ~ # ~ # ~

    @irpy.lazy_property
    def includes(self):
        # () -> str
        '''Extract the name of include who need be to be include in this Entity'''
        return [line.filename for _, line in self.d_type_lines[Include]]

    @irpy.lazy_property
    def uses(self):
        '''Extract the name of module who are used in this Entity'''
        return [line.filename for _, line in self.d_type_lines[Use]]

    @irpy.lazy_property
    def calls(self):
        '''Extract the name ofthe function called by the entity'''

        def extract_name(line):
            return line.text.split('(', 1)[0].split()[1].lower()

        return [extract_name(line) for _, line in self.d_type_lines[Call]]

    # ~ # ~ # ~
    # Array Dimension
    # ~ # ~ # ~

    @irpy.lazy_property
    def dim(self):
        # () -> List[str]
        '''Extract the dimension of the needed array in a form of list of variable name

	Exemple:
		BEGIN_PROVIDER [real, ao_num ]
		-> []

		BEGIN_PROVIDER [ real, ao_oneD_p, (ao_num) ]
		-> ['ao_num']

		BEGIN_PROVIDER [ real, ao_oneD_prim_p, (ao_num,ao_prim_num_max) ]
		-> ['ao_num', 'ao_prim_num_max']
	'''

        line = self.prototype.text.split('!')[0]
        buffer = line.replace(']', '').split(',', 2)

        try:
            x = buffer[2].strip()
        except IndexError:
            return []
        else:
            return map(str.strip, x[1:-1].split(','))

    @irpy.lazy_property
    def allocate(self):
        # () -> List[Str]
        '''Return a list of name of entity who are array and main'''
        if not self.is_main:
            return []
        else:
            # We never go here
            return [var for var in l_name if self.d_entity[var].dim]

    # ~ # ~ # ~
    # D e c l a r a t i o n
    # ~ # ~ # ~
    @irpy.lazy_property
    def is_protected(self):
        return self.text[0].lower.startswith('begin_provider_immu')

    @irpy.lazy_property
    def type(self):
        # () -> str
        '''Compute the fortran type code of the entity'''

        type_ = self.prototype.text.split(',')[0].split('[')[1].strip()

        if not type_:
            logger.error("Error in definition of %s." % (self.name))
            sys.exit(1)

        if self.dim:
            return "%s, allocatable" % (type_)
        else:
            return type_

    @irpy.lazy_property
    def d_header(self):
        # () -> List[str]
        '''Compute all the code needed to inistanticant the entity'''

        import util
        d_template = {
            'name': self.name,
            'type': self.type,
            'main': self.is_main,
            'dim': build_dim(
                self.dim, colons=True),
            'protected': '\n'.join(self.allocater + self.builder) if self.is_protected else False
        }
        return d_template

    ############################################################
    @irpy.lazy_property
    def fmodule(self):
        # () -> str
        '''Contruct the name of the module who will contain the entity'''
        name = self.prototype.filename[0].replace('/', '__').split('.irp.f')[0]
        return '%s_mod' % name

    ############################################################
    @irpy.lazy_property
    def regexp(self):
        # () -> Regex
        '''Compile a regex targeted to 'search' the name of this entity'''
        import re
        return re.compile(r"([^a-z0-9'\"_]|^)%s([^a-z0-9_]|$)" % (self.name), re.I).search

    # ~ # ~ # ~
    # F o r t r a n 9 0  G e n e r a t i o n
    # ~ # ~ # ~

    @irpy.lazy_property
    def d_touche_template(self):
        # () -> List[str]
        '''Fabric the f90 routine who handle the cache invalidation'''

        # Only one by EntityColleciton
        if not self.is_main:
            return {}

        from util import mangled

        return {
            'name': self.name,
            'l_module':
            [n for n in build_use(
                self.parents + [self.name], self.d_entity, use=False)],
            'l_ancestor': [n for n in mangled(self.parents, self.d_entity)]
        }

    ##########################################################

    @irpy.lazy_property
    def free(self):
        # () -> List[ str ]
        '''Compute an part of a subroutine used to free a variable'''

        name = self.name
        result = ["!", "! >>> FREE %s" % (name), "  %s_is_built = .False." % (self.same_as)]

        if self.dim:
            result += ["  if (allocated(%s)) then" % (name), "    deallocate (%s)" % (name)]
            if command_line.do_memory:
                result += "    print *, 'Deallocating %s'" % (name)

            result += ["  endif"]

        result.append("! <<< END FREE")
        return result

    ##########################################################
    @irpy.lazy_property
    def provider(self):
        # () -> List[str]
        '''Create the fortran90 code for the EntityCollection'''

        if not self.is_main:
            return []

        from util import mangled

        import util
        name = self.name
        l_module = [x for x in build_use([self.name] + self.to_provide, self.d_entity, use=False)]
        l_children = [x for x in mangled(self.to_provide, self.d_entity)]

        l_entity = [self.d_entity[n] for n in self.l_name]

        l = ashes_env.render('provider.f90', {
            'name': name,
            'l_module': l_module,
            'l_children_static': l_children,
            'do_debug': command_line.do_debug,
            'do_openmp': command_line.do_openmp,
            'do_task': command_line.do_Task,
            'do_corray': command_line.do_coarray,
            'dim': ','.join(self.dim),
            'l_entity': [{
                'name': i.name,
                'dim': ','.join(i.dim)
            } for i in l_entity]
        })
        return [i for i in l.split('\n') if i.strip()]

    @irpy.lazy_property
    def allocater(self):
        from util import mangled

        import util
        name = self.name
        l_module = [x for x in build_use([self.name] + self.to_provide, self.d_entity, use=False)]
        if self.is_protected:
            l_module.remove(self.fmodule)

        l_dim = [{'name': name, 'rank': i + 1, 'value': dimsize(k)} for i, k in enumerate(self.dim)]

        l = ashes_env.render('allocater.f90', {
            'name': name,
            'l_module': l_module,
            'do_debug': command_line.do_debug,
            'do_corray': command_line.do_coarray,
            'do_memory': command_line.do_memory,
            'dim': ','.join(self.dim),
            'l_dim': l_dim
        })
        return [i for i in l.split('\n') if i.strip()]

##########################################################

    @irpy.lazy_property
    def builder(self):
        if not self.is_main:
            return []

        # ~#~#~#~#~#
        # Get the raw text for the builder
        # ~#~#~#~#~#

        #Next return the first element of the iterator	
        ps_text = next(text for filename, text in self.cm_t_filename_parsed_text
                       if self.prototype.filename[0].startswith(filename))
        begin = next(i for i, (_, line) in enumerate(ps_text)
                     if isinstance(line, Begin_provider) if line.filename[1] == self.same_as)
        end = next(begin + i for i, (_, line) in enumerate(ps_text[begin:])
                   if isinstance(line, End_provider))

        # Now we now that the text is betern ps_text[begin:end]
        _, line_prototype = ps_text[begin]

        # ~#~#~#~#~#
        # Aded the call to the provider
        # ~#~#~#~#~#

        text = []
        if command_line.do_profile:
            text.append(([], Declaration(line_prototype.i,
                                         "  double precision :: irp_rdtsc, irp_rdtsc1, irp_rdtsc2",
                                         line_prototype.filename)))
            text.append(([], Simple_line(line_prototype.i, "  irp_rdtsc1 = irp_rdtsc()",
                                         line_prototype.filename)))

        for vars, line in ps_text[begin + 1:end]:

            text.append((vars, line))
            text += map(lambda x: ([], Simple_line(line.i, x, line.filename)),
                        build_call_provide(vars, self.d_entity))

        # ~#~#~#~#~#
        # Create the subroutine.
        # ~#~#~#~#~#

        result = []
        if command_line.directives and command_line.inline in ["all", "builders"]:
            result += ["!DEC$ ATTRIBUTES INLINE :: bld_%s" % (same_as)]

        # Add the use statement
        result += ["subroutine bld_%s" % (self.name)]

        l_use = build_use([self.name] + self.needs, self.d_entity, use=False)
        if self.is_protected:
            l_use.remove(self.fmodule)

        result += ['USE %s' % n for n in l_use]

        import parsed_text
        # Move the variable to top, and add the text
        parsed_text.move_to_top_list(text, [Declaration, Implicit, Use])

        result.extend(line.text for _, line in text
                      if not isinstance(line, (Begin_doc, End_doc, Doc, Cont_provider)))

        if command_line.do_profile:
            result += [
                "  irp_rdtsc2 = irp_rdtsc()",
                "  call irp_set_timer(%d,(irp_rdtsc2-irp_rdtsc1))" % self.label
            ]
        #Close
        result.append("end subroutine bld_%s" % (self.name))

        return result

    ##########################################################
    @irpy.lazy_property_mutable
    def needs(self):
        #Set by parsed_text.build_needs(...)
        raise AttributeError

    @irpy.lazy_property_mutable
    def needed_by(self):
        #Set by parsed_text.build_needs(...)
        return []

    @irpy.lazy_property
    def children(self):

        result = []
        for x in self.needs:
            result.append(x)
            try:
                result += self.d_entity[x].children
            except RuntimeError:
                pass  # Exception will be checked after

        result = OrderedUniqueList(result)
        if self.name in result:
            error.fail(self.prototype, "Cyclic dependencies:\n%s" % (str(result)))
        return result

    ##########################################################
    @irpy.lazy_property
    def parents(self):
        result = []
        for x in self.needed_by:
            result.append(x)
            result += self.d_entity[x].parents

        result = OrderedUniqueList(result)
        if self.name in result:
            error.fail(self.prototype, "Cyclic dependencies:\n%s" % (str(self._parents)))

        return result
