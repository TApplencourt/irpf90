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

from util import *
from irpf90_t import *
import regexps, re

re_string_sub = regexps.re_string.sub
regexps_re_string_sub = regexps.re_string.sub


def find_variables_in_line(line, vtuple):
    line_lower = regexps_re_string_sub('', line.lower)
    return [v for v, regexp in vtuple if v in line_lower and regexp(line_lower)]


def find_funcs_in_line(line, stuple):
    assert isinstance(line, Line)
    line_lower = line.lower
    result = [s for s, regexp in stuple if s in line_lower and regexp.search(line_lower)]
    return result


def find_subroutine_in_line(line):
    assert type(line) == Call
    buffer = line.text.split('(')[0]
    buffer = buffer.split()[1].lower()
    return buffer


def check_touch(variables, line, vars, main_vars):
    def fun(main_var):
        if main_var not in variables:
            error.fail(line, "Variable %s unknown" % (main_var, ))
        x = variables[main_var]
        return [main_var] + x.others_entity_name

    all_others = uniquify(flatten(map(fun, main_vars)))
    all_others.sort()
    vars.sort()
    for x, y in zip(vars, all_others):
        if x != y:
            message = "The following entities should be touched:\n"
            message = "\n".join([message] + map(lambda x: "- %s" % (x, ), all_others))
            error.fail(line, message)


from collections import namedtuple
Parsed_text = namedtuple('Parsed_text', ['varlist', 'line'])


################################################################################
def get_parsed_text(filename, text, variables, subroutines, vtuple):
    varlist = []
    result = []
    append = result.append

    for i, line in enumerate(text):

        tmp_result = []
        if isinstance(line,
                      (Empty_line, Continue, Return, Begin_shell, End_shell, Openmp, Directive, Use,
                       Enddo, End_select, Endif, End, Implicit, Program, Subroutine, Function)):

            append(Parsed_text([], line))

        elif isinstance(line, (Begin_provider, Cont_provider)):

            if isinstance(line, (Begin_provider)):
                varlist = []

            v = line.lower.replace(']', '').split(',')[1].strip()
            varlist.append(v)

            variable_list = find_variables_in_line(line, vtuple)
            variable_list.remove(v)

            append(Parsed_text(variable_list, line))

        elif type(line) == End_provider:
            varlist = []
            append(Parsed_text([], line))

        elif type(line) == Provide:
            l = line.lower.split()[1:]
            l = filter(lambda x: x not in varlist, l)
            for v in l:
                if v not in variables:
                    error.fail(line, "Variable %s is unknown" % (v))

            append(Parsed_text(l, Provide(line.i, "", line.filename)))
            append(Parsed_text(l, Simple_line(line.i, "!%s" % (line.text), line.filename)))

        elif type(line) == NoDep:
            l = line.lower.split()[1:]
            for v in l:
                if v not in variables:
                    error.fail(line, "Variable %s is unknown" % (v))
            l = map(lambda x: "-%s" % (x), l)
            append(Parsed_text(l, Simple_line(line.i, "!%s" % (line.text), line.filename)))
        elif type(line) in [Touch, SoftTouch]:

            vars = line.lower.split()
            if len(vars) < 2:
                error.fail(line, "Syntax error")
            vars = vars[1:]

            main_vars = uniquify([variables[x].same_as for x in vars])

            check_touch(variables, line, vars, main_vars)

            txt = " ".join(vars)
            append(Parsed_text(vars, Simple_line(line.i, "!", line.filename)))
            append(Parsed_text([], Simple_line(line.i, "! >>> TOUCH %s" % (txt, ), line.filename)))

            def fun(x):
                if x not in variables:
                    error.fail(line, "Variable %s unknown" % (x, ))
                return [
                    Parsed_text([], Simple_line(line.i, " call touch_%s" % (x, ), line.filename)),
                    Parsed_text([], Use(line.i, " use %s" % (variables[x].fmodule), line.filename))
                ]

            result += flatten(map(fun, main_vars))

            def fun(x):
                if x not in variables:
                    error.fail(line, "Variable %s unknown" % (x, ))
                return Parsed_text(
                    [], Simple_line(line.i, " %s_is_built = .True." % (x, ), line.filename))

            result += map(fun, main_vars[:-1])
            if type(line) == SoftTouch:
                append(
                    Parsed_text([], Simple_line(line.i, "! <<< END TOUCH (Soft)", line.filename)))
            else:
                append(Parsed_text([], Provide_all(line.i, "! <<< END TOUCH", line.filename)))

        elif type(line) == Call:
            l = find_variables_in_line(line, vtuple)
            l = filter(lambda x: x not in varlist, l)
            sub = find_subroutine_in_line(line)

            if sub not in subroutines:
                t = Simple_line
                append(Parsed_text(l, Simple_line(line.i, line.text, line.filename)))
            else:
                append((l, line))
                if subroutines[sub].touches != []:
                    append(Parsed_text([], Provide_all(line.i, "", line.filename)))

        elif type(line) == Free:
            vars = line.lower.split()
            vars = vars[1:]
            append(Parsed_text([], Simple_line(line.i, "!%s" % (line.text), line.filename)))
            use = map(lambda x: "  use %s" % (variables[x].fmodule), vars)
            for var in vars:
                result += map(lambda x: Parsed_text([], Use(line.i, x, line.filename)),
                              uniquify(use))
                result += map(lambda x: Parsed_text([], Simple_line(line.i, x, line.filename)),
                              variables[var].free)

        elif type(line) == Irp_read:
            append(Parsed_text([], Simple_line(line.i, "!%s" % (line.text), line.filename)))

        elif type(line) == Irp_write:
            append(Parsed_text([], Simple_line(line.i, "!%s" % (line.text), line.filename)))

        elif type(line) in [Begin_doc, End_doc, Doc]:
            pass
        else:
            l = find_variables_in_line(line, vtuple)
            l = filter(lambda x: x not in varlist, l)
            append(Parsed_text(l, line))

    return (filename, result)

######################################################################
def move_to_top_list(text, it):
    # ( List[ List[Entity], Line], Iterator)
    '''Move the all the element of List[ List[Entity], Line] of Line's Type containt in IT are the top of text.

    Note:
        - The permutation neeed to be done following `it` order
	- We can have `nested` subroutine / Function. (Because of interface)
	- This function is called way to much. Is need to be efficient
                      - This function is Unpure
     		      - One pass over `text`


   NB:
	- I am not really proud of the Sentinel value for the deleted,
	  but I waste already so much time on more cleaver but not working solution...
   '''

    assert set(it).issubset([NoDep, Declaration, Implicit, Use, Cont_provider])

    # ~ # ~ # ~
    # G e t  P e r m u t a t i o n
    # ~ # ~ # ~   

    from collections import defaultdict
    d_permutation = defaultdict(list)
    # Type:List(begin, Line)
    # We will insert the Line at begin position later on 

    l_begin = []

    for i, (l_var, line) in enumerate(text):
        t = type(line)
	
        if t in [Begin_provider, Module,Program, Subroutine, Function]:
            l_begin.append(i)
        elif t in [End_provider, End]:
	    l_begin.pop()

        elif l_begin and t in it:
            d_permutation[t].append( (l_begin[-1], [l_var, line]) )
	    # Put the sentinel, will be deleted after the insertion
	    text[i] = None	

    # ~ # ~ # ~
    # O r d e r  t h e m
    # ~ # ~ # ~   
    # We need to do the permutation in the correct order
    d_insert = defaultdict(list)
    for t in reversed(it):
        for begin, tline in d_permutation[t]:
            d_insert[begin].append(tline)

    # ~ # ~ # ~
    # D o  t h e m
    # ~ # ~ # ~   
    # Because idx are sorted, it's easy to do the insert part of the move
    # Just pad each insert by the number of precedent insert
    padding = 0
    for idx in sorted(d_insert.keys()):
        for tline in d_insert[idx]:
            text.insert(idx + padding + 1, tline)
            padding += 1

    # Now do the Delete part of the move. Fortunatly we put a sentinel to know the line to delete
    for i in reversed(xrange(len(text))):
	if text[i] is None:
		del text[i]


def move_interface(parsed_text,s_type=(Use,Implicit,Declaration,Subroutine,Function,Module)):
   # ( List[ List[Entity], Line], Iterator)
   '''Move everything containt into 'interface' below the first instance of s_type who preced it

   Note:
	= This function is unpur
   '''

   # Get the born of the interface	
   i_begin =  [ i   for i, (_,  line) in enumerate(parsed_text) if isinstance(line,Interface)  ]
   i_end   =  [ i+1 for i, (_, line) in  enumerate(parsed_text) if isinstance(line,End_interface) ]

   # Get the begin of the insert
   i_insert = [] 
   for begin in i_begin:
	i_insert.append(next(i+1 for i in range(begin,-1,-1) if isinstance(parsed_text[i][1], s_type)))

    # Do the insert and the delete in one passe
   for insert, begin, end in zip(i_insert,i_begin,i_end):
		parsed_text[insert:insert]  = parsed_text[begin:end]

		padding = end-begin
		parsed_text[begin+padding:end+padding] = []	

######################################################################
def build_sub_needs(parsed_text, d_subroutine):
    #(List[ Tuple[List[Entity], Tuple[int,List[Line]] ]], Dict[str:Sub]) -> None
    '''Set the needs, and provides arguements of Routine present in parsed_text
    
    Note:
	This function is unpure	
    '''

    l_buffer = []
    for _, text in parsed_text:
        l_begin = [ i for i, (_, line) in enumerate(text) if isinstance(line, (Subroutine, Function, Program))]
        l_end = [i for i, (_, line) in enumerate(text) if isinstance(line, End)]

        l_buffer += [(d_subroutine[text[b].line.subname], text[b + 1:e]) for b, e in zip(l_begin, l_end) if not isinstance(text[b].line, Program)]

    for sub, text in l_buffer:
        sub.needs = set(v for vs, _ in text for v in vs)
        sub.to_provide = set(v for vs, line in text for v in vs if isinstance(line, Declaration))

#####################################################################


def add_subroutine_needs(parsed_text, subroutines):
    main_result = []
    for filename, text in parsed_text:
        result = []
        append = result.append
        for vars, line in text:
            if type(line) == Call:
                vars += subroutines[line.subname].to_provide
            append((vars, line))
        main_result.append((filename, result))
    return main_result


######################################################################
def move_variables(parsed_text):
    #(List[ Tuple[List[Entity], Tuple[int,List[Line]] ]]
    '''Move variables into the top of the declaraiton'''

  
    def func(filename, text):
        result = []
        append = result.append
        # 1st pass
        varlist = []
        ifvars = []
        elsevars = []
        old_varlist = []
        old_ifvars = []
        old_elsevars = []
        revtext = list(text)
        revtext.reverse()
  
	skip_interface = False
        try:
            for vars, line in revtext:
		if type(line) in [Interface, End_interface]:
			skip_interface = not skip_interface
		
		if skip_interface:
			append(([], line))
			continue

                if type(line) in [End_provider, End]:
                    varlist = []
                    append(([], line))
                elif type(line) in [Endif, End_select]:
                    old_ifvars.append(list(ifvars))
                    old_elsevars.append(list(elsevars))
                    old_varlist.append(list(varlist))
                    varlist = []
                    append(([], line))
                elif type(line) == Else:
                    elsevars += list(varlist)
                    append((varlist, line))
                    varlist = []
                elif type(line) in [Elseif, Case]:
                    ifvars += list(varlist)
                    append((varlist, line))
                    if vars != []:
                        varlist = old_varlist.pop()
                        varlist += vars
                        old_varlist.append(list(varlist))
                    varlist = []
                elif type(line) in [If, Select]:
                    ifvars += list(varlist)
                    append((varlist, line))
                    vars += filter(lambda x: x in elsevars, ifvars)
                    ifvars = old_ifvars.pop()
                    elsevars = old_elsevars.pop()
                    varlist = old_varlist.pop() + vars
                elif type(line) in [Begin_provider, Program, Subroutine, Function]:
                    varlist += vars
                    append((varlist, line))
                    if old_varlist != [] \
                     or old_ifvars != [] \
                     or old_elsevars != []:
                        error.fail(line, "End if missing")
                    varlist = []
                elif type(line) in (Provide, Provide_all):
                    append((vars, line))
                else:
                    varlist += vars
                    append(([], line))
        except:
	    from util import logger
            logger.error("Unable to parse file %s", line)
	    import sys
	    sys.exit(1)

        result.reverse()

        # 2nd pass
        text = result
        result = []
        append = result.append
        old_varlist = []
        varlist = []
        try:
            for vars, line in text:
                if vars:
                    vars = uniquify(vars)
                if type(line) in [Begin_provider, Program, Subroutine, Function]:
                    varlist = list(vars)
                elif type(line) in [If, Select]:
                    old_varlist.append(varlist)
                    vars = filter(lambda x: x not in varlist, vars)
                    varlist = uniquify(varlist + vars)
                    assert old_varlist is not varlist
                elif type(line) in [Elseif, Else, Case]:
                    varlist = old_varlist.pop()
                    old_varlist.append(varlist)
                    vars = filter(lambda x: x not in varlist, vars)
                    varlist = uniquify(varlist + vars)
                    assert old_varlist is not varlist
                elif type(line) in [Endif, End_select]:
                    varlist = old_varlist.pop()
                elif type(line) == Provide_all:
                    vars += varlist
                elif type(line) in [End_provider, End]:
                    assert old_varlist == []
                    varlist = []
                for v in vars[:]:
                    if v[0] == '-':
                        vars.remove(v)
                        vars.remove(v[1:])
                result.append((vars, line))
        except:
            error.fail(line, "Unable to parse file")
        return result

    main_result = []
    for filename, text in parsed_text:
        main_result.append((filename, func(filename, text)))
    return main_result


######################################################################
def build_needs(parsed_text, subroutines, stuple, variables):
    'out: variable'

    # ~#~#~#~#~#
    # Needs and to_provide
    # ~#~#~#~#~#

    # Loop of the main Entity
    for filename, text in parsed_text:

        l_begin = [i for i, (_, line) in enumerate(text) if isinstance(line, Begin_provider)]
        l_end = [i for i, (_, line) in enumerate(text) if isinstance(line, End_provider)]

        for start, end in zip(l_begin, l_end):
            l_vars_start, line_start = text[start]

            name = map(str.strip, line_start.lower.replace(']', ',').split(','))[1]
            entity = variables[name]

            entity.to_provide = uniquify(l_vars_start)

            l_needs = []
            for vars, line in text[start:end]:
                l_needs += vars

                if type(line) == Call:
                    l_needs += subroutines[line.subname].needs
                elif type(line) in [Simple_line, Assert, Do, If, Elseif, Select]:
                    funcs = find_funcs_in_line(line, stuple)
                    for f in funcs:
                        l_needs += subroutines[f].needs

            entity.needs = uniquify(l_needs)


    # Now do the Other entity
    for v in variables:
        main = variables[v].same_as
        if main != v:
            variables[v].needs = variables[main].needs
            variables[v].to_provide = variables[main].to_provide

    # ~#~#~#~#~#
    # Needs_by
    # ~#~#~#~#~#

    # This a some dark vodou magic.
    # The algo is:
    #    - Initialise needed_by
    #    - Create the pointer copy
    #   -  Add the value (so it add also to the pointer reference...)
    
    for v in variables:
        main = variables[v].same_as
        if main != v:
            variables[v].needed_by = variables[main].needed_by

    for v in variables:
        var = variables[v]
        if var.is_main:
            for x in var.needs:
                variables[x].needed_by.append(var.same_as)

    for var in variables.values():
        var.needed_by = uniquify(var.needed_by)

######################################################################
from command_line import command_line


def check_opt(parsed_text):
    if not command_line.do_checkopt:
        return

    for filename, text in parsed_text:
        do_level = 0
        for vars, line in text:
            if not type(line) == Provide_all:
                if do_level > 0 and vars != []:
                    print "Optimization: %s line %d" % (line.filename, line.i)
                    for v in vars:
                        print "  PROVIDE ", v
                if type(line) == Do:
                    do_level += 1
                elif type(line) == Enddo:
                    do_level -= 1


######################################################################
def perform_loop_substitutions(parsed_text):
    main_result = []
    for filename, text in parsed_text:
        result = []
        append = result.append
        for vars, line in text:
            if type(line) in [Do, If, Elseif]:
                for k, v in command_line.substituted.items():
                    reg = v[1]
                    while reg.search(line.text) is not None:
                        line.text = re.sub(reg, r'\1%s\3', line.text, count=1) % v[0]
            append((vars, line))
        main_result.append((filename, result))
    return main_result

