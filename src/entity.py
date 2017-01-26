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
try:
    import irpy
except:
    import lib_irpy as irpy


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
	return next(line for _,line in d[Begin_provider]+d[Cont_provider] if line.filename[1] == self.name)


    @irpy.lazy_property
    def others_entity_name(self):
	# () -> List[str]
	'''Extract the other entity-name defined'''
	d = self.d_type_lines
	return [line.filename[1] for _,line in d[Begin_provider]+d[Cont_provider] if not line.filename[1] == self.name]


    @irpy.lazy_property
    def doc(self):
	# () -> List[str]
        doc = [line.text.lstrip()[1:] for _,line in self.d_type_lines[Doc]]
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
    def writer(self):
        if not self.is_main:
            result = []
        else:
            name = self.name
            result = [ \
            "subroutine writer_%s(irp_num)"%(name),
            "  use %s"%(self.fmodule),
            "  implicit none",
            "  character*(*), intent(in) :: irp_num",
            "  logical                   :: irp_is_open",
            "  integer                   :: irp_iunit" ]
            if command_line.do_debug:
                length = len("writer_%s" % (self.name))
                result += [\
                "  character*(%d) :: irp_here = 'writer_%s'"%(length,name),
                "  call irp_enter(irp_here)" ]
            result += [ \
            "  if (.not.%s_is_built) then"%(self.same_as),
            "    call provide_%s"%(self.same_as),
            "  endif" ]
            result += map(lambda x: "  call writer_%s(irp_num)" % (x), self.needs)
            result += [ \
            "  irp_is_open = .True.",
            "  irp_iunit = 9",
            "  do while (irp_is_open)",
            "   irp_iunit = irp_iunit+1",
            "   inquire(unit=irp_iunit,opened=irp_is_open)",
            "  enddo" ]
            for n in [name] + self.others_entity_name:
                result += [\
                "  open(unit=irp_iunit,file='irpf90_%s_'//trim(irp_num),form='FORMATTED',status='UNKNOWN',action='WRITE')"%(n),
                "  write(irp_iunit,*) %s%s"%(n,build_dim(self.d_entity[n].dim,colons=True)),
                "  close(irp_iunit)" ]
            if command_line.do_debug:
                result.append("  call irp_leave(irp_here)")
            result.append("end subroutine writer_%s" % (name))
            result.append("")
        return result

    @irpy.lazy_property_mutable
    def is_read(self):
	'''Check if it  will be read from disk'''
        return any(self.d_entity[i].is_read for i in self.parents)

    @irpy.lazy_property
    def reader(self):
        if not self.is_main:
            result = []
        else:
            name = self.name
            result = [ \
            "subroutine reader_%s(irp_num)"%(name),
            "  use %s"%(self.fmodule),
            "  implicit none",
            "  character*(*), intent(in) :: irp_num",
            "  logical                   :: irp_is_open",
            "  integer                   :: irp_iunit" ]
            if command_line.do_debug:
                length = len("reader_%s" % (self.name))
                result += [\
                "  character*(%d) :: irp_here = 'reader_%s'"%(length,name),
                "  call irp_enter(irp_here)" ]
            result += map(lambda x: "  call reader_%s(irp_num)" % (x), self.needs)
            result += [ \
            "  irp_is_open = .True.",
            "  irp_iunit = 9",
            "  do while (irp_is_open)",
            "   inquire(unit=irp_iunit,opened=irp_is_open)",
            "  enddo"]
            for n in [name] + self.others:
                result += [\
                "  open(unit=irp_iunit,file='irpf90_%s_'//trim(irp_num),form='FORMATTED',status='OLD',action='READ')"%(n),
                "  read(irp_iunit,*) %s%s"%(n,build_dim(self.cm_d_variable[n].dim,colons=True)),
                "  close(irp_iunit)" ]
            result += [ \
            "  call touch_%s"%(name),
            "  %s_is_built = .True."%(name) ]
            if command_line.do_debug:
                result.append("  call irp_leave(irp_here)")
            result.append("end subroutine reader_%s" % (name))
            result.append("")
        return result

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
        return [line.filename for _,line in self.d_type_lines[Include]]

    @irpy.lazy_property
    def uses(self):
      '''Extract the name of module who are used in this Entity'''  
      return [line.filename for _,line in self.d_type_lines[Use]]

    @irpy.lazy_property
    def calls(self):
	'''Extract the name ofthe function called by the entity'''

	def extract_name(line):
		return line.text.split('(', 1)[0].split()[1].lower()

	return [extract_name(line) for _,line in self.d_type_lines[Call] ]

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
            return [var for var in self.others_entity_name + [self.name] if self.d_entity[var].dim]

    # ~ # ~ # ~
    # D e c l a r a t i o n
    # ~ # ~ # ~

    @irpy.lazy_property
    def type(self):
	# () -> str
	'''Compute the fortran type code of the entity'''

        type_ = self.prototype.text.split(',')[0].split('[')[1].strip()

	if not type_:
	    logger.error( "Error in definition of %s." % (self.name))	 
      	    sys.exit(1)

	if self.dim:
            return "%s, allocatable" % (type_)
        else:
            return type_

    @irpy.lazy_property
    def header(self):
	# () -> List[str]
	'''Compute all the code needed to inistanticant the entity'''


        name = self.name
        str_ = "  {type_} :: {name} {dim}".format(type_=self.type, name=name, dim=build_dim(self.dim, colons=True))

        if command_line.coarray:
            if not self.dim:
                str_ += " [*]"
            else:
                str_ += " [:]"

	l = [str_]
        if self.dim and command_line.align != '1':
            l += ["  !DIR$ ATTRIBUTES ALIGN: %s :: %s" % (command_line.align, name)]

        if self.is_main:
            l += ["  logical :: %s_is_built = .False." % (name)]

        return l


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
    def toucher(self):
	# () -> List[str]
	'''Fabric the f90 routine who handle the cache invalidation'''

	# Only one by EntityColleciton
        if not self.is_main:
            return []

        parents = self.parents
        name = self.name

        result = ["subroutine touch_%s" % (name)]

        result += build_use(parents+[name],self.d_entity)
        result.append("  implicit none")

        if command_line.do_debug:
            length = str(len("touch_%s" % (name)))
            result += ["  character*(%s) :: irp_here = 'touch_%s'" % (length, name)]
            result += ["  call irp_enter(irp_here)"]

        result += map(lambda x: "  %s_is_built = .False." % (x), parents)
        result.append("  %s_is_built = .True." % (name))

        if command_line.do_debug:
            result.append("  call irp_leave(irp_here)")

        result.append("end subroutine touch_%s" % (name))
        result.append("")

        return result

    ##########################################################
    @irpy.lazy_property
    def locker(self):
            if not command_line.do_openmp:
               return []

            name = self.name
            result = ["subroutine irp_lock_%s(set)" % (name)]
            result += [
                "  use omp_lib",
                "  implicit none",
                "  logical, intent(in) :: set",
                "  integer(kind=omp_nest_lock_kind),save :: %s_lock" % (name),
                "  integer,save :: ifirst",
            ]
            if command_line.do_debug:
                length = str(len("irp_lock_%s" % (name)))
                result += [
                    "  character*(%s) :: irp_here = 'irp_lock_%s'" % (length, name),
                    "  call irp_enter(irp_here)"
                ]

            result += [
                "  if (ifirst == 0) then",
                "    ifirst = 1",
                "    call omp_init_nest_lock(%s_lock)" % (name),
                "  endif",
                "  if (set) then",
                "    call omp_set_nest_lock(%s_lock)" % (name),
                "  else",
                "    call omp_unset_nest_lock(%s_lock)" % (name),
                "  endif",
            ]
            if command_line.do_debug:
                result.append("  call irp_leave(irp_here)")
            result.append("end subroutine irp_lock_%s" % (name))
            result.append("")
            return result

    ##########################################################
    @irpy.lazy_property
    def free(self):
	# () -> List[ str ]
	'''Compute an part of a subroutine used to free a variable'''

        name = self.name
        result = ["!", 
                  "! >>> FREE %s" % (name), 
		  "  %s_is_built = .False." % (self.same_as)]

        if self.dim:
            result += [
                "  if (allocated(%s)) then"%(name),
                "    deallocate (%s)"%(name)]
            if command_line.do_memory:
		result += "    print *, 'Deallocating %s'"%(name)

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

        name = self.name
        same_as = self.same_as

        def dimsize(x):
	    # (str) -> str
	    '''Compute the number of element in the array'''
	    try:
	        b0, b1 = x.split(':')
	    except ValueError:
		return x

	    b0_is_digit =  b0.replace('-', '').isdigit()
	    b1_is_digit =  b1.replace('-', '').isdigit() 
	   

            if b0_is_digit and b1_is_digit:
                size = str(int(b1) - int(b0) + 1)
            elif b0_is_digit:
                    size = "(%s) - (%d)" % (b1, int(b0) - 1)
            elif b1_is_digit:
                    size = "(%d) - (%s)" % (int(b1) + 1, b0)
            else:
                    size = "(%s) - (%s) + 1" % (b1, b0)

            return size

        def build_alloc(name):

            var = self.d_entity[name]
            if var.dim == []:
                return []

            from util import build_dim

            def print_size():
                return " " * 5 + "print *, ' size: {0}'".format(build_dim(var.dim))

            def check_dimensions():
                l = ["(%s>0)" % dimsize(x) for x in var.dim]
                str_ = ".and.".join(l)
                return "   if (%s) then" % (str_)

            def dimensions_OK():
                result = ["  irp_dimensions_OK = .True."]
                for i, k in enumerate(var.dim):
                    result.append("  irp_dimensions_OK = irp_dimensions_OK.AND.(SIZE(%s,%d)==(%s))"
                                  % (name, i + 1, dimsize(k)))
                return result

            def do_allocate():
                if command_line.coarray:
                    result = "    allocate(%s(%s)[*],stat=irp_err)"
                else:
                    result = "    allocate(%s(%s),stat=irp_err)"
                result = result % (name, ','.join(var.dim))
                if command_line.do_memory:
                    tmp = "\n   print *, %s, 'Allocating %s(%s)'"
                    d = ','.join(self.dim)
                    result += tmp % ('size(' + name + ')', name, d)
                return result

            result = [" if (allocated (%s) ) then" % (name)]
            result += dimensions_OK()
            result += [
                "  if (.not.irp_dimensions_OK) then", "   deallocate(%s,stat=irp_err)" % (name),
                "   if (irp_err /= 0) then", "     print *, irp_here//': Deallocation failed: %s'" %
                (name), print_size(), "   endif"
            ]

            if command_line.do_memory:
                result += ["   print *, 'Deallocating %s'" % (name)]
            result.append(check_dimensions())
            result.append(do_allocate())
            result += [\
              "    if (irp_err /= 0) then",
              "     print *, irp_here//': Allocation failed: %s'"%(name),
              print_size(),
              "    endif",
              "   endif",
              "  endif",
              " else" ]
            result.append(check_dimensions())
            result.append(do_allocate())
            result += [
                "    if (irp_err /= 0) then", "     print *, irp_here//': Allocation failed: %s'" %
                (name), print_size(), "    endif", "   endif", " endif"
            ]
            return result

        result = []
        if command_line.directives and command_line.inline in ["all", "providers"]:
            result += ["!DEC$ ATTRIBUTES FORCEINLINE :: provide_%s" % (name)]
        result += ["subroutine provide_%s" % (name)]
        result += build_use([same_as] + self.to_provide, self.d_entity)
        if command_line.do_openmp:
            result += [" use omp_lib"]
        result.append("  implicit none")
        length = len("provide_%s" % (name))
        result += [
            "  character*(%d) :: irp_here = 'provide_%s'" % (length, name),
            "  integer                   :: irp_err ",
            "  logical                   :: irp_dimensions_OK",
            "!$ integer                  :: nthreads"
        ]
        if command_line.do_openmp:
            result.append(" call irp_lock_%s(.True.)" % (same_as))
        if command_line.do_assert or command_line.do_debug:
            result.append("  call irp_enter(irp_here)")
        result += build_call_provide(self.to_provide, self.d_entity)
        result += flatten(map(build_alloc, [self.same_as] + self.others_entity_name))
        result += [
            " if (.not.%s_is_built) then" % (same_as), "  call bld_%s" % (same_as),
            "  %s_is_built = .True." % (same_as), ""
        ]
        result += [" endif"]
        if command_line.do_assert or command_line.do_debug:
            result.append("  call irp_leave(irp_here)")
        if command_line.do_openmp:
            result.append(" call irp_lock_%s(.False.)" % (same_as))
        result.append("end subroutine provide_%s" % (name))
        result.append("")
        return result

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
        result += build_use([self.name] + self.needs, self.d_entity)

        import parsed_text
        # Move the variable to top, and add the text
	parsed_text.move_to_top_list(text, [Declaration, Implicit, Use])

        result.extend( line.text for _,line in text if not isinstance(line, (Begin_doc, End_doc, Doc, Cont_provider)))

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
        if not self.is_main:
            return []

        result = []
        for x in self.needed_by:
            result.append(x)
            try:
                result += self.d_entity[x].parents
            except RuntimeError:
                pass  # Exception will be checked after

        result = OrderedUniqueList(result)
        if self.name in result:
            error.fail(self.prototype, "Cyclic dependencies:\n%s" % (str(self._parents)))

        return result
