#!/unr/bin/env python
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
from command_line import command_line
import preprocessed_text
from util import *
from entity import Entity

def put_info(text, filename):

    lenmax = 80 - len(filename)
    format_ = "%" + str(lenmax) + "s ! %s:%4s"

    str_ = '{text:{width}} ! {filename}:{i:4}'
    for _, line in text:
	line.text = str_.format(text=line.text,filename=line.filename,i=line.i,width=lenmax)
    return text


class Fmodule(object):

    header = [ "! -*- F90 -*-", 
               "!",
               "!-----------------------------------------------!",
               "! This file was generated with the irpf90 tool. !",
               "!                                               !",
               "!           DO NOT MODIFY IT BY HAND            !",
               "!-----------------------------------------------!", 
	       ""]

    def __init__(self, text, filename, d_variable):
        self.text = put_info(text, filename)
        self.filename = filename[:-6]
        self.name = "%s_mod" % (self.filename).replace('/', '__').replace('.', 'Dot')
	self.d_all_variable = d_variable

    @irpy.lazy_property
    def prog_name(self):
        l = [line.filename for _, line in self.text if isinstance(line, Program)]
        return l.pop() if l else None

    @irpy.lazy_property
    def is_main(self):
        return self.prog_name is not None

    @irpy.lazy_property
    def l_entity(self):
        return [value for value in self.d_all_variable.values() if value.fmodule == self.name]

    @irpy.lazy_property
    def head(self):
        '''The module who containt the declaration of the entity'''
        body = list(self.use)
	body += list(self.dec)
        body += [header for var in self.l_entity for header in var.header]


	if body:
		result = ["module %s" % (self.name)]
		result += body
	        result += ["end module %s" % (self.name)]
	else:
		result = []

        return result

    @irpy.lazy_property
    def has_irp_module(self):
	return bool(self.head)

    @irpy.lazy_property
    def needed_vars(self):
        return set(n for var in self.l_entity for n in var.needs)

    @irpy.lazy_property
    def includes(self):
        return set(i for var in self.needed_vars for i in self.d_all_variable[var].includes)

    @irpy.lazy_property
    def generated_text(self):
	'Routine genereraed by the IRPF90. provide, build, ...'
        result = []
        for var in self.l_entity:
            result += var.provider
            result += var.builder
            if var.is_read:
                result += var.reader
            if var.is_written:
                result += var.writer
	
        return result

    @irpy.lazy_property
    def residual_text_use_dec(self):

        def remove_providers(text):
            result = []
            inside = False

            for vars, line in text:
                if type(line) == Begin_provider:
                    inside = True
                if not inside:
                    result.append((vars, line))
                if type(line) == End_provider:
                    inside = False
            return result

        def modify_functions(text):

            result = []
            variable_list = []
	    skip_interface = False
            for vars, line in text:
		if type(line) in [Interface, End_interface]:
			skip_interface = not skip_interface

		if skip_interface:
			result.append((vars, line))
			continue


                if type(line) in [Subroutine, Function, Program]:		
                    #Deep copy...	
                    variable_list = list(vars)
                elif type(line) == End:
                    result += [([], Use(line.i, x, line.filename)) for x in build_use(variable_list, self.d_all_variable)]
                else:
                    variable_list += vars

                result.append((vars, line))
            return result

        def extract_use_dec_text(text):
	    # (List[ Tuple(Entity,Line) ]) -> (List[ Tuple(Entity,Line),List[ Tuple(Entity,Line),List[ Tuple(Entity,Line))
	    '''Extract the global declaration statement and module use form the declaration of function. '''

            inside = 0
            result,dec,use,module = [],[],[],[]

            for vars, line in text:
		
                if isinstance(line, (Subroutine, Function, Program,Interface,Module)):
                    inside += 1

		if type(line) == Module:
			module.append((vars,line))

                if inside:
                    result.append((vars, line))
                else:
                    if type(line) == Use:
                        use.append((vars, line))
                    elif type(line) == Declaration:
                        dec.append((vars, line))
			

                if isinstance(line,(End,End_interface,End_module)):
		    inside += -1
		    
	    if inside:
		print 'Something wrong append'
		sys.exit(1)

            return use, module, dec, result

        result = remove_providers(self.text)
        result = modify_functions(result)
        
        from collections import namedtuple
        Residual_text_use_dec = namedtuple('Residual_text_use_dec', ['use', 'module', 'dec', 'result'])

        return Residual_text_use_dec(*extract_use_dec_text(result))

    @irpy.lazy_property
    def use(self):
        return set(" %s" % line.text for _, line in self.residual_text_use_dec.use)

    @irpy.lazy_property
    def gen_mod(self):
	'''List of module generated by the user in this module...'''
        return set("%s" % line.subname for _, line in self.residual_text_use_dec.module)

    @irpy.lazy_property
    def dec(self):
	'''The declaration of this module
	
	Note:
		Because user can define F90 Type, we need to keep the correct order.
	
	Warning:
		If we uniquify that can cause a problem with the type in guess.
		```type toto
			integer :: n
		   end type toto
		   integer :: n
		```
	Fix:
        	We need to support Type keyword.

	'''

	l = [" %s" % line.text for _, line in self.residual_text_use_dec.dec]
	from util import uniquify
	if len(l) != len(uniquify(l)):
		raise NotImplementedError

        return l

    @irpy.lazy_property
    def residual_text(self):
        '''Not the generated function (build, provide, touch, etc.)'''

        result = []
        for vars, line in self.residual_text_use_dec.result:
            result.append(([], line))
            result += map(lambda x: ([], Simple_line(line.i, x, line.filename)),
                          build_call_provide(vars, self.d_all_variable))


        from parsed_text import move_to_top_list, move_interface
        move_to_top_list(result, [Declaration, Implicit, Use])
	move_interface(result)

        return  [line.text for _, line in result]

    @irpy.lazy_property
    def needed_modules(self):
	l = set(x.split(',only').pop(0).split()[1] for x in self.generated_text + self.head + self.residual_text if x.lstrip().startswith("use "))

        if self.name in l:
            l.remove(self.name)

        return l

	
    @irpy.lazy_property
    def needed_modules_irp(self):
        return [i for i in self.needed_modules if i.endswith("_mod")]

    @irpy.lazy_property
    def needed_modules_usr(self):
	return [i for i in self.needed_modules if not i.endswith("_mod")]


