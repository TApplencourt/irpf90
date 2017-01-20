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
from regexps import *
from command_line import command_line
from util import *
import sys

# Local regular expressions
re_endif = re.compile("end +if")
re_elseif = re.compile("else +if")
re_enddo = re.compile("end +do")
re_endtype = re.compile("end +type.*")
re_endmodule = re.compile("end +module",re.I)
re_endselect = re.compile("end +select")
re_endinterface = re.compile("end +interface")

# Local variables
Free_form = 0
Fixed_form = 1

######################################################################
# Dictionary of simple statements
simple_dict = {
    "program": Program,
    "subroutine": Subroutine,
    "begin_shell": Begin_shell,
    "end_shell": End_shell,
    "begin_template": Begin_template,
    "end_template": End_template,
    "subst": Subst,
    "end_doc": End_doc,
    "begin_provider": Begin_provider,
    "&begin_provider": Cont_provider,
    "end_provider": End_provider,
    "assert": Assert,
    "touch": Touch,
    "soft_touch": SoftTouch,
    "provide": Provide,
    "no_dep": NoDep,
    "free": Free,
    "irp_if": Irp_If,
    "irp_else": Irp_Else,
    "irp_endif": Irp_Endif,
    "irp_read": Irp_read,
    "irp_write": Irp_write,
    "use": Use,
    "do": Do,
    "if": If,
    "case": Case,
    "elseif": Elseif,
    "else": Else,
    "enddo": Enddo,
    "endif": Endif,
    "endselect": End_select,
    "end": End,
    "include": Include,
    "call": Call,
    "continue": Continue,
    "return": Return,
    "implicit": Implicit,
    "save": Declaration,
    "function": Function,
    "recursive": Function,
    "select": Select,
    "selectcase": Select,
    "module": Module,
    "endmodule": End_module,
    "interface": Interface,
    "endinterface": End_interface
}

def get_canonized_text(text_lower):

    text_canonized = text_lower
    text_canonized = re_elseif.sub("elseif", text_canonized)
    text_canonized = re_enddo.sub("enddo", text_canonized)
    text_canonized = re_endtype.sub("endtype", text_canonized)
    text_canonized = re_endmodule.sub("endmodule", text_canonized)
    text_canonized = re_endif.sub("endif", text_canonized)
    text_canonized = re_endselect.sub("endselect", text_canonized)
    text_canonized = re_endinterface.sub("endinterface", text_canonized)

    for c in """()'"[]""":
        text_canonized = text_canonized.replace(c, " %s " % c)
    return text_canonized


def get_type(i, filename, line, line_lower, line_lower_canonized, is_doc):
    # ( int,str,str,str,str,bool) -> Irpf90_t	
    '''Find the type of a text line'''
    
    line = line.rstrip()
    l_word = line_lower_canonized.split()

    if not l_word:
        return [Empty_line(i, line, filename)], is_doc

    # Handle archaic do loop of f77
    firstword = l_word[0]
    if firstword.isdigit():
	l_word = l_word[1:]
        firstword = l_word[0]

    if firstword == "contains":
	return [Contains(i, line, filename)], False
    if firstword == "end_doc":
        return [End_doc(i, line, filename)], False

    if firstword == "begin_doc":
        return [Begin_doc(i, line, filename)], True

    if is_doc:
        return [Doc(i, line, filename)], is_doc

    if firstword in simple_dict:
        type_ = simple_dict[firstword]
        return [type_(i, line, filename)], is_doc


    #label do-loop (outer: do i=1,sze)
    reg_do_lab = ur":\s+do\s+"
    if re.search(reg_do_lab,line_lower):
	return [Do(i,line,filename)], is_doc
   	

    lower_line = line_lower.strip()[1:]

    if len(lower_line) <= 3:
        return [Simple_line(i, line, filename)], is_doc

    #WARNING: The regex shloud match endtype. Don't know why it's not the case
    if re_decl.match(line_lower_canonized) or line_lower_canonized == 'endtype':
        if "function" in l_word[1:3]:
            return [Function(i, line, filename)], is_doc
        else:
            return [Declaration(i, line, filename)], is_doc

    if firstword.startswith('#'):
        result = [Simple_line(i, line, filename)]

        logger.info("%s:"
		    "irpf90 may not work with preprocessor directives. You can use"
                    "Irp_if ... Irp_else ... Irp_endif"
                    "instead of"
                    "#ifdef ... #else ... #endif"%line)
        return result, is_doc

    if firstword.startswith("case("):
        return [Case(i, line, filename)], is_doc

    if lower_line.startswith("$omp"):
        return [Openmp(i, line, filename)], is_doc
    if lower_line.startswith(("dec$", "dir$")) and command_line.directives:
        return [Directive(i, line, filename)], is_doc
    if lower_line.startswith("$ "):
        return [Openmp(i, line, filename)], is_doc

    # Detect errors
    if firstword == "dowhile":
        logger.error("%s 'do while' should be in 2 words." % Do(i, line, filename))
	sys.exit(1)

    return [Simple_line(i, line, filename)], is_doc

######################################################################
import os.path


def save_and_execute(irpdir, scriptname, code, interpreter):
    # (str, str, List, str) -> List[Line]
    ''' Save the script in irpdir/scriptname and Execute it

    Note:
	The script are executed in the orginal directory of the .irp.f (aka '..')
	and this directory is added to PYTHONPATH.
    '''

    irpdir_scriptname = os.path.abspath(os.path.join(irpdir, scriptname))
    with open(irpdir_scriptname, 'w') as f:
        f.writelines(code)

    # Execute shell
    import util
    try:
	text = util.check_output('PYTHONPATH=$PYTHONPATH:. %s %s' % (interpreter, irpdir_scriptname), shell=True, bufsize=-1, cwd=os.path.join(irpdir,'..'))
    except:
	util.logger.error("Something wrong append with embeded '%s' script:  %s"% (interpreter, irpdir_scriptname))
	import sys
	sys.exit(1)

    # Create the Line
    p = Preprocess_text(scriptname)
    p.text = text
    return p.lines_overloaded


#######################################################################
def execute_shell(text):
    # (List[Line]) -> List[Line]
    '''Execute the embedded shell scripts'''


    l_begin = [i for i,line in enumerate(text) if isinstance(line,Begin_shell)]
    l_end   = [i for i,line in enumerate(text) if isinstance(line,End_shell)]
    l_output= []

    # ~=~=~=~
    # E x e c u t e  S h e l l 
    # ~=~=~=~
    from util import logger
    import sys
    def fail(l, a, b):
        logger.error("%s In Begin_Shell, %s '%s'" % (l,a, b))
	sys.exit(1)

    for begin,end in zip(l_begin,l_end):

	header = text[begin]
	header_text = header.text

        for bracket in ['[', ']']:
              n = header_text.count(bracket)
              assert n <= 1, fail(header_text, "Too many", bracket)
              assert n >= 1, fail(header_text, "Missing", bracket)
	else:
	      interpreter = header_text[header_text.find('[')+1: header_text.find(']')].strip()
	script = ['%s\n' % l.text for l in text[begin+1:end] ]
	scriptname="%s_shell_%d" % (header.filename, header.i)

        l_output.append(save_and_execute(irpdir, scriptname, script,interpreter))

    # ~=~=~=~
    # R e p l a c e
    # ~=~=~=~

    #Deep copy for pure function
    text_new = text[:]

    # Because we use slicing and we want to include the end line
    l_end_include = [i+1 for i in l_end]
    padding = 0
    for begin,end, out in zip(l_begin,l_end_include,l_output):
	text_new[begin+padding:end+padding] = out
	padding += len(out) - (end-begin)
	
    return text_new


######################################################################
def execute_templates(text):
    '''Execute the templates'''

    def fail(l, a, b):
        error.fail(l, "In %s, %s" % (a, b))

    def get_variables(line):
        buffer = line.text.split('[', 1)
        if len(buffer) < 2:
            fail(line, "Subst", "Syntax error")
        buffer = buffer[1].replace(']', '')
        buffer = buffer.split(',')
        return map(lambda x: '$%s' % (x.strip()), buffer)

    TEMPLATE = 1
    SUBST = 2
    inside = 0
    result = []
    for line in text:
        if inside == 0:
            if type(line) == Begin_template:
                script = []
                inside = TEMPLATE
                script = "template = \"\"\"\n"
            else:
                result.append(line)
        elif inside == TEMPLATE:
            if type(line) == Begin_template:
                fail(line, "template", "Nested Begin_Template")
            elif type(line) == End_template:
                fail(line, "template", "Missing Subst")
            elif type(line) == Subst:
                inside = SUBST
                script += "\"\"\"\n"
                variables = get_variables(line)
                script += "v = []\n"
                subst = ""
            else:
                script += line.text + "\n"
        else:  # inside == SUBST
            if type(line) == Begin_template:
                fail(line, "subst", "Nested Begin_template")
            elif type(line) == Subst:
                fail(line, "subst", "Subst already defined")
            elif type(line) == End_template:
                inside = 0
                subst = subst.rstrip()
                if subst[-2:] == ';;':
                    subst = subst[:-2]
                for s in subst.split(';;'):
                    buffer = map(lambda x: x.strip(), s.split(';'))
                    if len(buffer) != len(variables):
                        fail(line, "subst", "%d variables defined, and %d substitutions" %
                             (len(variables), len(buffer)))
                    script += "v.append( { \\\n"
                    for t, v in zip(variables, buffer):
                        script += ' "%s": """%s""" ,\n' % (t, v)
                    script += "} )\n"
                script += "for d in v:\n  t0 = str(template)\n"
                for v in variables:
                    script += "  t0 = t0.replace('%s',d['%s'])\n" % (v, v)
                script += "  print t0\n"
                result += save_and_execute(irpdir, scriptname="%s_template_%d" % (line.filename, line.i), code=script,interpreter="python")
            else:
                subst += line.text + '\n'

    return result


######################################################################
def form(text):
    '''Find if the text is in fixed form or in free form'''
    assert type(text) == list
    if len(text) == 0:
        return Free_form
    assert isinstance(text[0], Line)

    re2 = re.compile(r"^\s*[!#]")
    re3 = re.compile(r"^\s*[^ 0-9]+")
    for line in text:
        if type(line) in [Empty_line, Doc, Openmp, Directive]:
            pass
        else:
            if len(line.text) > 5:
                test = line.text[0:5]
                if test[0] in "Cc#!*":
                    pass
                else:
                    if re2.match(test) is None and re3.match(test) is not None:
                        return Free_form
                    if line.text.rstrip()[-1] == '&':
                        return Free_form
    return Fixed_form


######################################################################
def add_operators(text):
    re_incr = re.compile(r"(\s*)(.*)(\+=)(.*$)", re.S)
    re_decr = re.compile(r"(\s*)(.*)(-=)(.*$)", re.S)
    re_mult = re.compile(r"(\s*)(.*)(\*=)(.*$)", re.S)
    '''Change additional operators'''
    result = []
    for line in text:
        buffer = line.text
        ls = buffer.strip()
        if ls.startswith('print ') or \
           ls.startswith('print*') or \
           ls.startswith('write('):
            pass
        elif "+=" in buffer:
            if buffer.lstrip().startswith("if "):
                re_incr = re.compile(r"(.*)(\))(\s*)(.*)(\+=)(.*$)", re.S)
                line.text = re.sub(re_incr, r'\1\2\4=\4+(\6)', buffer)
            else:
                line.text = re.sub(re_incr, r'\1\2=\2+(\4)', buffer)
        elif "-=" in buffer:
            line.text = re.sub(re_decr, r'\1\2=\2-(\4)', buffer)
        elif "*=" in buffer:
            line.text = re.sub(re_mult, r'\1\2=\2*(\4)', buffer)
        result.append(line)
    return result


######################################################################
def remove_comments(text, form):
    # (List[Line], int) -> List[Line]
    '''Remove all comments

    Note:
	This function is unpur
    '''
    result = []

    def remove_after_bang(str_):
	# str -> str
	i_bang = str_.find('!')
	
	if i_bang == -1:
		return str_
	else:
		sentinel, inside  = None, False
		for i,c in enumerate(str_):
			if c == '"' or c == "'":
				if not inside:
					inside = True
					sentinel = c
				elif sentinel == c:
					inside = False

			elif c == '!' and not inside:
				return str_[:i]
				
		return str_
	

    if form == Free_form:
        for line in text:
            if type(line) in [Openmp, Doc, Directive]:
                result.append(line)
            elif type(line) == Empty_line:
                pass
            else:
                newline = line.text.lstrip()
                if (newline != "" and newline[0] != "!#"):
		    text = remove_after_bang(line.text)
		    if text:
        	        line.text = text 
    	                result.append(line)

        return result
    else:
        for line in text:
            if type(line) in [Openmp, Doc, Directive]:
                result.append(line)
            elif type(line) == Empty_line:
                pass
            else:
                newline = line.text.lstrip()
                if newline == "" or newline[0] == "!":
                    pass
                else:
                    line.text = remove_after_bang(line.text)
                    if line.text[0] in "#123456789 ":
                        result.append(line)
        return result


######################################################################
def remove_continuation(text, form):
    '''Removes continuation lines'''
    result = []
    buffer = ""
    number = 0
    t = None
    if form == Free_form:
        for line in text:
            if line.text[-1] == '&':
                buffer = "%s%s\n" % (buffer, line.text)
                if number == 0:
                    t = type(line)
                    number = line.i
            else:
                if number != 0:
                    newline = t(number, \
                      "%s%s"%(buffer,line.text), \
                      line.filename)
                    line = newline
                    number = 0
                    buffer = ""
                result.append(line)
    else:
        rev_text = list(text)
        rev_text.reverse()
        for line in rev_text:
            is_continuation = False
            if type(line) == Simple_line:
                if len(line.text) >= 6:
                    if line.text[5] != ' ':
                        is_continuation = True
            if is_continuation:
                buffer = "&\n%s %s %s" % (line.text[:5], line.text[6:], buffer)
            else:
                line.text = line.text + buffer
                result.insert(0, line)
                buffer = ""
    return result


######################################################################
def irp_simple_statements(text):
    '''Processes simple statements'''

    def process_irp_rw(line, rw, t):
	'''Read Write'''
        assert type(line) == t
        buffer = line.text.split()
        if len(buffer) == 2:
            dummy, variable = buffer
            num = "0"
        elif len(buffer) == 3:
            dummy, variable, num = buffer
        else:
            error.fail(line, "Error in IRP_%s statement" % (rw, ))
        variable = variable.lower()
        i = line.i
        f = line.filename
        txt = line.text.lstrip()
        result = [
            Empty_line(i, "!", f),
            t(i, "! >>> %s" % txt, variable),
            Provide_all(i, "   call %ser_%s('%s')" % (rw, variable, num), f),
            Empty_line(i, "! >>> END %s " % (txt, ), f),
            Empty_line(line.i, "!", f),
        ]
        return result

    def process_irp_read(line):
        assert type(line) == Irp_read
        return process_irp_rw(line, 'read', Irp_read)

    def process_irp_write(line):
        assert type(line) == Irp_write
        return process_irp_rw(line, 'writ', Irp_write)

    def process_return(line):
        assert type(line) == Return
        if command_line.do_assert or command_line.do_debug:
            newline = Simple_line(line.i, " call irp_leave(irp_here)", line.filename)
            result = [newline, line]
        else:
            result = [line]
        return result

    def debug_conditions(line):
        '''Find condition in assert statement for debug'''
        assert type(line) == Assert
        match = re_test.search(line.text)
        result = []
        if match is not None:
            matches = [match.group(1).strip(), match.group(3).strip()]
            for m in matches:
                ok = m != ""  # not empty
                ok = ok and not m.isdigit()  # not a digit
                ok = ok and "'" not in m  # not a string
                ok = ok and m.count('(') == m.count(')')  # balanced parenthesis
                if ok:
                    result.append(
                        Simple_line(line.i, " print *, '%s = ', %s" % (m, m), line.filename))
            result.append(Simple_line(line.i, " print *, ''", line.filename))
        return result

    def process_assert(line):
        assert type(line) == Assert
        if command_line.do_assert:
            if '(' not in line.text or ')' not in line.text:
                error.fail(line, "Syntax error in ASSERT statement (parentheses)")
            condition = "(%s" % (line.text.split('(', 1)[1])
            if condition == "":
                error.fail(line, "Error in Assert statement")
            condition_str = condition.replace("'", "''")
            i = line.i
            f = line.filename
            txt = line.text.strip()
            result = [
                Empty_line(i, "!", f),
                Empty_line(i, "! >>> %s" % (txt, ), f),
                If(i, "  if (.not.%s) then" % (condition, ), f),
                Simple_line(i, "   call irp_trace", f),
                Simple_line(i, "   print *, irp_here//': Assert failed:'", f),
                Simple_line(i, "   print *, ' file: %s, line: %d'" % (f, i), f),
                Simple_line(i, "   print *, '%s'" % (condition_str, ), f),
            ] + debug_conditions(line) + [
                Simple_line(i, "   stop 1", f), Endif(i, "  endif", f), Empty_line(
                    i, "! <<< END %s" % (txt, ), f), Empty_line(i, "!", f)
            ]
        else:
            result = []
        return result

    def process_end(line):
        '''Add irp_leave if necessary'''

        if command_line.do_assert or command_line.do_debug:
            i = line.i
            f = line.filename
            result = [Simple_line(i, " call irp_leave(irp_here)", f), line]
        else:
            result = [line]
        return result

    def process_begin_provider(line):
        assert type(line) == Begin_provider
        import string
        trans = string.maketrans("[]","  ")
        buffer = line.lower.translate(trans).split(',')

        if len(buffer) < 2:
            error.fail(line, "Error in Begin_provider statement")
        varname = buffer[1].strip()
        length = len(varname)
        i = line.i
        f = line.filename
        result = [
            Begin_provider(i, line.text, (f, varname)),
            Declaration(i, "  character*(%d) :: irp_here = '%s'" % (length, varname), f)
        ]
        if command_line.do_assert or command_line.do_debug:
            result += [Simple_line(i, "  call irp_enter(irp_here)", f), ]
        return result

    def process_cont_provider(line):
        assert type(line) == Cont_provider
        buf = line.lower.replace('[', " ").replace(']', "").split(',')
        if len(buf) < 2:
            error.fail(line, "Error in Cont_provider statement")
        varname = buf[1].strip()
        i = line.i
        f = line.filename
        return [Cont_provider(i, line.text, (f, varname))]

    def process_subroutine(line):
        assert type(line) == Subroutine
        subname = line.subname
        length = len(subname)
        i = line.i
        f = line.filename
        result = [ line, Declaration(i, "  character*(%d) :: irp_here = '%s'" % (length, subname), f)]

        if command_line.do_assert or command_line.do_debug:
            result += [Simple_line(i, "  call irp_enter_f(irp_here)", f), ]
        return result

    def process_function(line):
        assert type(line) == Function
	subname = line.subname
        length = len(subname)
        i = line.i
        f = line.filename
        result = [
            line, Declaration(i, "  character*(%d) :: irp_here = '%s'" % (length, subname), f)
        ]
        if command_line.do_assert or command_line.do_debug:
            result += [Simple_line(i, "  call irp_enter_f(irp_here)", f), ]
        return result

    def process_program(line):
        assert type(line) == Program
        program_name = line.lower.split()[1]
        temp = [Program(0, "program irp_program", program_name)]
        if command_line.do_profile:
            temp += [Simple_line(0, "call irp_init_timer()", line.filename)]
        if command_line.do_openmp:
            temp += [Simple_line(0, " call irp_init_locks_%s()" % (irp_id), line.filename)]
        temp += [Call(0, " call %s" % (program_name), line.filename)]
        if command_line.do_profile:
            temp += [Simple_line(0, "call irp_print_timer()", line.filename)]

        temp += [Simple_line(0, " call irp_finalize_%s()" % (irp_id), line.filename)]
        temp += [End(0, "end program", line.filename)]

        result = temp + process_subroutine(
            Subroutine(line.i, "subroutine %s" % (program_name, ), line.filename))
        return result

    d = {
        Irp_read: process_irp_read,
        Irp_write: process_irp_write,
        Return: process_return,
        Assert: process_assert,
        End: process_end,
        Begin_provider: process_begin_provider,
        Cont_provider: process_cont_provider,
        End_provider: process_end,
        Subroutine: process_subroutine,
        Function: process_function,
        Program: process_program,
    }


    result = []
    for line in text:
        buffer = [line]
        for t in d:
            if type(line) == t:
                buffer = d[t](line)
                break
        result += buffer

    return result


######################################################################
def change_includes(text):
    '''Deals with include files'''
    result = []
    for line in text:
        if type(line) == Include:
            txt = line.text.replace('"', "'").split("'")
            if len(txt) != 3:
                error.fail(line, "Error in include statement")
            directory = (("./" + line.filename).rsplit('/', 1)[0] + '/')[2:]
            if directory == "":
                filename = txt[1].strip()
            else:
                filename = directory + txt[1].strip()
            try:
                result.append(Include(line.i, "! include '%s'" % filename, filename))
                result += Preprocess_text(filename).preprocessed_text
            except IOError:
                result.append(Declaration(line.i, line.text, line.filename))
        else:
            result.append(line)
    return result


######################################################################
def process_old_style_do(text):
    # (List[Line]) -> List[Line]
    '''Changes archaic do loops to new style
     DO 1 i=1,10'''

    def change_matching_enddo(begin, number):
	for i,line in enumerate(text[begin+1:]):
		if isinstance(line,(Continue,Enddo)) and line.text.split()[0] == number:
                    text[begin+1+i] = Enddo(line.i, "  enddo", line.filename)
                    return

	from util import logger
        logger.error(text[begin], "(%s) Old-style do loops should end with 'continue' or 'end do'" % text[begin])
	from util import sys
	sys.exit(1)

    result = []
    for i in range(len(text)):
        line = text[i]
        if type(line) == Do:
            buffer = line.text.split()
            try:
                if buffer[1].isdigit():
                    number = buffer.pop(1)
                    change_matching_enddo(i, number)
                    line.text = " ".join(buffer)
            except IndexError:
                pass
        result.append(line)
    return result


######################################################################
def change_single_line_ifs(text):
    # List[Line] -> List[Line]
    '''Changes:	`if (test) result`
	into
                `if (test) then
                    result
                 endif`'''

    regex = r"\(.*?\)"

    result = []
    for line in text:
        if type(line) == If:
            if line.lower.endswith("then"):
                result.append(line)
            else:
                buffer = line.text
                begin = buffer.find('(')
	        if begin == -1:
			logger.error("No '(' in if statemnt: %s" % line)
			sys.exit(1)

                level = 0
                instring = False
                for i, c in enumerate(buffer[begin:]):
                    if c == "'":
                        instring = not instring
                    if instring:
                        pass
                    elif c == '(':
                        level += 1
                    elif c == ')':
                        level -= 1
                    if level == 0:
                        end = begin + i + 1
                        break
                if level != 0:
                    logger.error("If statement not valid: %s (%s)" % (line, line.filename))
		    sys.exit(1)

                test = buffer[:end]
                code = buffer[end:]
                i = line.i
                f = line.filename
                result.append(If(i, "%s then" % (test, ), f))
                result += get_type(i, f, code, code.lower(),code.lower(), False)[0]
                result.append(Endif(i, "  endif", f))
        else:
            result.append(line)
    return result


######################################################################
def check_begin_end(raw_text):
    # List[Line] -> None
    '''Checks x...endx consistence


    Note:
      fortran 'ifdef' statement may cause this function to bug.
      Indeed we count the numboer of 'x' statement and compare it to the number of 'endx'.
      Maybe more of one 'x' statement in defined cause in 'ifdef/else/endif' statement.
    '''

    d_block = {Enddo: [Do],
	       Endif: [If],
	       End_provider: [Begin_provider],
	       End_doc: [Begin_doc],
	       End: [Program, Subroutine, Function],
	       End_module: [Module],
	       End_interface: [Interface]}

    from collections import defaultdict
    d_type = defaultdict(list)

    for line in raw_text:
	d_type[type(line)].append(line)
    
    for t_end, l_begin in d_block.iteritems():
	n_end = len(d_type[t_end])
	n_begin = sum(len(d_type[t_begin]) for t_begin in l_begin)
  
	if n_end > n_begin:
	    logger.error("You have more close statement than open statement (%s) (%s)",line.filename,t_end)
	    sys.exit(1)
	elif n_end < n_begin:
	    logger.error('You have more end statement than open statenemt for (%s)  (%s)' %  (line.filename, t_end))
	    sys.exit(1)

######################################################################
def remove_ifdefs(text):
    assert type(text) == list
    result = []

    do_print = True
    for line in text:
        if type(line) == Irp_If:
            var = line.text.split()[1]
            do_print = var in command_line.defined
        elif type(line) == Irp_Else:
            do_print = not do_print
        elif type(line) == Irp_Endif:
            do_print = True
        elif do_print:
            result.append(line)

    return result


######################################################################
def check_OpenMP(text):
    assert type(text) == list
    inside_openmp = False
    for line in text:
        if type(line) == Openmp:
            # Detect OpenMP blocks
            buffer = line.lower.split()

            if buffer[1] == "parallel":
                inside_openmp = True
            elif buffer[1] == "end" and buffer[2] == "parallel":
                inside_openmp = False

        if inside_openmp and isinstance(line, (Provide_all, Provide, Touch, SoftTouch)):
                logger.error("%s is not allowed in an OpenMP block: %s" % (type(line),line))

    return text


######################################################################
class Preprocess_text(object):
    def __init__(self, filename):
        self.filename = filename

    @irpy.lazy_property_mutable
    def text(self):
        with open(self.filename, 'r') as f:
            str_ = f.read()
	
	#Dirty thing. We will replace 'end program' by 'end subroutine'
	#because afterward the program will be replaced by a subroutine...	

	import re
	transform = re.compile(re.escape('end program'), re.IGNORECASE)
	
	return transform.sub('end subroutine', str_)

    @irpy.lazy_property_mutable
    def text_align(self):
        from command_line import command_line
        return self.text.replace("$IRP_ALIGN", command_line.align)

    @irpy.lazy_property
    def text_lower(self):
        return self.text_align.lower()

    @irpy.lazy_property
    def text_lower_canonized(self):
        return get_canonized_text(self.text_lower)

    @irpy.lazy_property
    def lines(self):
        return self.text_align.split('\n')

    @irpy.lazy_property
    def lines_lower(self):
        return self.text_lower.split('\n')

    @irpy.lazy_property
    def lines_lower_canonized(self):
        return self.text_lower_canonized.replace("!", " ! ").split('\n')

    @irpy.lazy_property
    def lines_overloaded(self):
        '''Labeled lines'''
        result = []
        is_doc = False

        for i, (l, ll, llc) in enumerate(zip(self.lines, self.lines_lower, self.lines_lower_canonized)):
            line, is_doc = get_type(i + 1, self.filename, l, ll, llc, is_doc)
            result += line
        return result

    @irpy.lazy_property
    def preprocessed_text(self):
        result = self.lines_overloaded
        result = execute_templates(result)
        result = execute_shell(result)
        fortran_form = form(result)
        result = remove_ifdefs(result)
        result = remove_comments(result, fortran_form)

        result = remove_continuation(result, fortran_form)
        result = add_operators(result)
        result = change_includes(result)
        result = change_single_line_ifs(result)

        result = process_old_style_do(result)
        result = irp_simple_statements(result)
        result = check_OpenMP(result)

        check_begin_end(result)

        return result


if __name__ == '__main__':
    debug()
