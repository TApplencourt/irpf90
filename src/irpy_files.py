from util import parmap,lazy_write_file
from util import flatten, listdir

try:
  import irpy
except:
  import lib_irpy as irpy

import os
import irpf90_t
import sys

from command_line import command_line

from util import logger

class Irpy_comm_world(object):
    '''Maestro.'''

    def __init__(self,l_dir=None, l_file=None):
	# (Iter, Iter) -> None

        # Create directories
	from itertools import ifilterfalse
        i_folder = ifilterfalse(os.path.exists, (irpf90_t.irpdir, irpf90_t.mandir))
        map(os.mkdir,i_folder)

	# List file
	
	l_dir =l_dir if l_dir else (command_line.include_dir+['.'])
	l_not_dir = [d for d in l_dir if not (os.path.exists(d) and os.path.isdir(d))]
	if l_not_dir:
		logger.error('Try to include no existing directory: [%s]' % ','.join(l_not_dir))
		sys.exit(1)
	
	# Create folder in IRPDIR
	i_folder = ifilterfalse(os.path.exists, (os.path.join(irpf90_t.irpdir,d) for d in l_dir))
	map(os.mkdir, i_folder)

	s_folder_abs = set(os.path.abspath(path) for path in l_dir)
   
	s_file_folder_all = set(flatten(listdir(path,abspath=True) for path in s_folder_abs))

	# Take everything!
	s_file_folder = filter(lambda f: os.path.isfile(f) and not f.startswith("."), s_file_folder_all)


 	

	s_file_tot = set(l_file) if l_file else set() 
	s_file_tot.update(s_file_folder)
	
	s_file_rel = set(os.path.relpath(f,self.cwd) for f in s_file_tot)

	# Lazy Copy file
	for f in s_file_rel:
	     src = os.path.join(self.cwd,f)	
             text_ref = open(src, 'rb').read()

             dest = os.path.join(self.cwd,irpf90_t.irpdir, f)
             lazy_write_file(dest, text_ref)		

        if command_line.do_codelet:
                s_file_tot.update(command_line.codelet[3])

	# No filter the irpf90 file
	self.irpf90_files_ordered=sorted(filter(lambda f: f.endswith(".irp.f") ,s_file_rel))

    @irpy.lazy_property
    def cwd(self):
	return os.getcwd()

    @irpy.lazy_property
    def t_filename_preprocessed_text(self):
	'''Tuple (filename, preprocessed_text)'''

        from preprocessed_text import Preprocess_text
        def worker_preprocess(filename):
            return (filename, Preprocess_text(filename).preprocessed_text)

        return parmap(worker_preprocess, self.irpf90_files_ordered)

    @irpy.lazy_property
    def l_preprocessed_text(self):
	# (None) -> List[Line]
	'''List preprocessed_text'''

        return [line for _, text in self.t_filename_preprocessed_text for line in text]

    @irpy.lazy_property
    def d_type_lines(self):

        from collections import defaultdict
        d = defaultdict(list)
        for i, line in enumerate(self.l_preprocessed_text):
            d[type(line)] += [(i, line)]
        return d

    @irpy.lazy_property
    def d_entity(self):
	# None -> Dict[Str,Entity]
        '''And entity is a collection of line between BEGIN_PROVIDER and END_PROVIDER '''
        from irpf90_t import Begin_provider, End_provider
        from entity import Entity

        l_begin = [i for i, line in self.d_type_lines[Begin_provider]]
        l_end =   [i for i, line in self.d_type_lines[End_provider]]
        l_provider = [ self.l_preprocessed_text[begin:end] for begin, end in zip(l_begin, l_end)]

        d_ent = dict()
        for icount, buf in enumerate(l_provider):
            v = Entity(buf, icount, comm_world=self)
            d_ent[v.name] = v
            for other in v.others_entity_name:
                d_ent[other] = Entity(buf, icount, other, comm_world=self)

        #
        # Second pass
        #
        # Modify parameter of variables

        d_modif = dict()

        # Touch Softouch	
        def find_variable(line):
            l_var = line.lower.split()[1:]
            if len(l_var) < 1:
                error.fail(line, "Syntax error")

            if any(v for v in l_var if v not in d_ent):
                error.fail(line, "Variable %s unknown" % (v, ))
            return l_var

        from irpf90_t import Touch, SoftTouch, Free
        from util import flatten
        for cmd, l_type in [('is_self_touched', [Touch, SoftTouch]),
                            ('is_free', [Free])]:

            l_line = flatten( [self.d_type_lines[type_] for type_ in l_type])
            l_name = flatten( [find_variable(line) for _, line in l_line])
            d_modif[cmd] = l_name

        # Read and Write
        from irpf90_t import Irp_read, Irp_write
        for cmd, type_ in [('is_read', Irp_read), ('is_written', Irp_write)]:
            l_name = [line.filename for _, line in self.d_type_lines[type_]]
            d_modif[cmd] = l_name

        #Do the modifcation
        for cmd, l_name in d_modif.items():
            for name in l_name:
                setattr(d_ent[name], cmd, True)

        return d_ent

    @irpy.lazy_property_mutable
    def d_routine(self):
        '''
         Dictionnary name -> Routine object.
         Routine is a collection of line between Subroutine / Function
         '''


        # ~#~#~#~#~#
        # Create the dict
        # ~#~#~#~#~#

        from irpf90_t import Subroutine, Function, Program, End
	d_type = self.d_type_lines
        l_begin = sorted(i for type_ in (Subroutine, Function, Program) for i, _ in d_type[type_])
        l_end = [i for i, _ in d_type[End]]

	from routine import Routine
	text = self.l_preprocessed_text
        l_rou = [ Routine(text[b:e]) for b, e in zip(l_begin, l_end) if not isinstance(text[b], Program)]

        # Now we can create a dict and at it
        d_rou = dict()
        for s in l_rou:
            d_rou[s.name] = s

        # ~#~#~#~#~#
        # Finish the initialization of the routine
        # ~#~#~#~#~#

        from collections import defaultdict
        d_called_by = defaultdict(set)

        for entity in self.d_entity.values():
            name = entity.name
            if entity.same_as == name:
                for x in entity.calls:
                    d_called_by[x].add(name)

	from util import uniquify
	for routine in d_rou.values():
	    for x in routine.calls:
		 d_called_by[x].add(routine.name)

	for routine in d_rou.values():
	    routine.called_by = sorted(d_called_by[routine.name])
 
	    l_set = [d_rou[name].touches_my_self for name in routine.calls if name in d_rou]
	    routine.touches_ancestor = set().union(*l_set)

        return d_rou

    @irpy.lazy_property
    def t_filename_parsed_text(self):
	'''(filename,parsed_text)'''
        import parsed_text
        d_entity = self.d_entity
        d_routine = self.d_routine

        # ~ # ~ # ~
        # F i r s t  R o u n d
        # ~ # ~ # ~   
        import parsed_text
	vtuple = [(v, s.same_as, s.regexp) for v, s in d_entity.iteritems()]
        def worker_parsed(filename_text):
            filename, text = filename_text
            return parsed_text.get_parsed_text(filename, text, d_entity, d_routine, vtuple)

        parsed_text_0 = parmap(worker_parsed, self.t_filename_preprocessed_text)

        #Touch routine
        parsed_text.build_sub_needs(parsed_text_0, d_routine)
        parsed_text.parsed_moved_to_top(parsed_text_0)

        parsed_text_1 = parsed_text.add_subroutine_needs(parsed_text_0, d_routine)
        parsed_text_1 = parsed_text.move_variables(parsed_text_1)
        
	parsed_text.parsed_moved_to_top(parsed_text_1)

        parsed_text.check_opt(parsed_text_1)
        parsed_text_1 = parsed_text.perform_loop_substitutions(parsed_text_1)

        #touch entity
	stuple = [(s, v.regexp) for s, v in d_routine.iteritems()  if v.is_function]
        parsed_text.build_needs(parsed_text_1, d_routine, stuple,d_entity)

        return parsed_text_1

    @irpy.lazy_property
    def d_module(self):
        from module import Fmodule
        result = dict()
        for filename, text in self.t_filename_parsed_text:
            result[filename] = Fmodule(text, filename,self.d_entity)

        return result

    def write_modules(self):
        from irpf90_t import irpdir
        from util import lazy_write_file
        import os

        for m in self.d_module.values():
            # Module data
            if m.has_irp_module:
              filename = os.path.join(irpdir, '%s.irp.module.F90' % m.filename) 
	      text = '\n'.join(m.header + m.head)
              lazy_write_file(filename, '%s\n' % text)

            # Subroutines
            filename = os.path.join(irpdir, '%s.irp.F90' % m.filename)
            text = '\n'.join(m.header + m.generated_text + m.residual_text)
            lazy_write_file(filename, '%s\n' % text)

    def create_stack(self):
        import irp_stack
        irp_stack.create()

    def create_buildfile(self,ninja):
	import build_file
        build_file.run(self.d_module,ninja)

    def create_touches(self):
        import touches
        touches.create(self.d_module, self.d_entity)

    def create_man(self):
        import create_man as c_man
        c_man.run(self.d_entity, self.d_routine)


    def create_lock(self):
	  from util import lazy_write_file
	  l = sorted(self.d_entity.keys())

	  out = []
	  for v in l:
             out += self.d_entity[v].locker

          out += [ "subroutine irp_init_locks_%s()"%(irpf90_t.irp_id),
                    " implicit none" ]
          for v in l:
              out += [ "  call irp_lock_%s(.True.)"%v ]
              out += [ "  call irp_lock_%s(.False.)"%v ]
          out += [ "end subroutine", "" ]

	  filename = os.path.join(irpf90_t.irpdir,'irp_locks.irp.F90')
  	  lazy_write_file(filename, '\n'.join(out))

