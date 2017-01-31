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


# ~#~#~#~#~#
#  L o g e r
# ~#~#~#~#~#
'''
Level     Numeric value
CRITICAL     50
ERROR        40
WARNING      30
INFO         20
DEBUG        10
NOTSET        0
'''
import logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('Irpf90')
logger.setLevel(30)

# ~#~#~#~#~#
#  / /  _  R E L A T E D
# ~#~#~#~#~#

def chunkify(l,n_chunk):
    # (List[any], int) -> List [ List[any] ]
    '''Split the list on n_chunk'''
    len_ = len(l)
    n = max(1,  len_ / n_chunk )
    return [ l[i:i + n] for i in xrange(0, len_, n) ]


import multiprocessing
def parmap(f, it, parallel=False):
    # (Callable, Iterable, bool) -> List
    '''Parallel version of the std map function
    
    The parallel flag is set to togle the // execusion
	
    Note:
	- We try to use the Mulprocesses map is possible else we use our own
	- The order of the sequence if concerved
        - Will use all the processesor possible
	- We return a List
	- The traceback is loose if an error occur but a Exception is raise.
    '''

    if not parallel:
       return map(f, it)

    nproc = multiprocessing.cpu_count()

    # ~!~!~!
    # Parallelisation STD
    # ~!~!~

    # This 'hang' on Travis and I don't know why...
    # https://docs.python.org/2/library/pickle.html#what-can-be-pickled-and-unpickled
    #from cPickle import PicklingError
    #try:
    #	 p = multiprocessing.Pool(nproc)
    #  	 l_res = p.map(f, it,nproc)
    #except PicklingError:
    #	pass	
    #else:
    #	return l_res

    # ~!~!~!
    # Parallelisation By Us
    # ~!~!~

    # To optimize the // performance,
    # we merge the task into chunk
    # In this implementation, we minimizise the communication
    # (aka 1 job by processor)

    it_chunk = chunkify(l=it,n_chunk=nproc)
    def F(chunk):
        # (List[any]) -> (List[any])
        '''Same as 'f' but for a chunck'''
        return map(f,chunk)
	

    q_in = multiprocessing.JoinableQueue()
    q_out = multiprocessing.Queue()
    # All the worker will sepuku after reseaving this message
    # Remove that we need to always put in `q_in` a not None value.
    stop_condition = None

    def worker():
       # () -> None
       '''Read a task from q_in, excute it, and store it in q_out

       Note:
	  - We use 'F' and not 'f'. 
	  - The for loop will break when stop_contition occur
	  - We get, and put an idx to allow the possibility of ordering afterward
	  - We store any exeception, to raise her afterward
       '''
       for i, x in iter(q_in.get, stop_condition):

	    try:
		result = F(x)
	    except BaseException as e:
		t = e
            else:	
	   	t = (i, result)

	    q_out.put(t)
            q_in.task_done()

       q_in.task_done()

    # Process' creation
    l_proc = [multiprocessing.Process(target=worker) for _ in range(nproc)]
    for p in l_proc:
        p.daemon = True
        p.start()

    # Add the job to the queue (Note we add an idx, this will all)
    for i, x in enumerate(it_chunk):
	q_in.put((i, x))

    # Now add the stop contidion and join
    # (Because q_in.get is blocking we don't need to join the queue before)
    for _ in l_proc:
        q_in.put(stop_condition)
    q_in.join()

    # Get all the chunk and join the process
    l_res = [q_out.get() for _ in range(len(it_chunk))]   

    for p in l_proc:
        p.join()
    
    # Check if error have occured  
    try:
	from itertools import ifilter
	e = next(ifilter(lambda t: isinstance(t,BaseException), l_res))
    except StopIteration:
   	# Now we need first to order the result, and secondly to flatte it
    	return [item for _, chunk in sorted(l_res) for item in chunk]
    else:
	raise e

# ~#~#~#~#~#
#  I O  _  R E L A T E D
# ~#~#~#~#~#
import hashlib
import os
def cached_file(filename, text):
    # (str,str) -> bool
    '''Check if file locatte at filename containt the same data as text

    Return:
	True if data is the same, false otherwise
    '''

    def digest(data):
	# (str) -> str
	'''compute an uniq data id'''
	return  hashlib.md5(data).hexdigest()

    try:
	text_ref = open(filename, 'rb').read()
    except IOError:
        return False
    else:
        return digest(text_ref) == digest(text)


def lazy_write_file(filename, text, conservative=False,touch=False):
    # (str, str, bool) -> None
    '''Write data lazily in filename location.

    Note:
	If convervative is set, we don't overwrite.
    '''

    if not os.path.exists(filename) or not cached_file(filename, text) and not conservative:
        with open(filename, 'w') as f:
            f.write(text)
    elif touch:
	os.utime(filename,None)

def listdir(directory, abspath=False):
    #(str, bool) -> List[str]
    '''Replacement of the std:listdir but with the possibility to get the abosulte path'''

    l_filename = os.listdir(directory)
    if not abspath:
        return l_filename
    else:
        return [os.path.abspath(os.path.join(directory, f)) for f in l_filename]

def check_output(*popenargs, **kwargs):
    """Run command with arguments and return its output as a byte string.
    Backported from Python 2.7 as it's implemented as pure python on stdlib.
    >>> check_output(['/usr/bin/python', '--version'])
    Python 2.6.2
    """
    import subprocess
    process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
    output, unused_err = process.communicate()
    retcode = process.poll()
    if retcode:
        cmd = kwargs.get("args")
        if cmd is None:
            cmd = popenargs[0]
        error = subprocess.CalledProcessError(retcode, cmd)
        error.output = output
        raise error
    return output

# ~#~#~#~#~#
#  L i s t 
# ~#~#~#~#~#


def uniquify(l,sort=False):
    # (Iter, bool) -> List[Any]
    '''Uniquify a immutable iterable. Don't preserve the order'''
    r = list(set(l))
    if not sort:
	return r
    else:
	return sorted(r)

def OrderedUniqueList(l):
    # (Iter, bool) -> List[Any]
    '''Uniquify a immutable iterable. Don't preserve the order'''
    return sorted(set(l))

def flatten(l_2d):
    # (List [ List[Any] ]) -> List
    '''Construct a copy of the 2d list collapsed into one dimension.

    Note:
	-  We collapse in a C-style fashion (row_major).
    '''

    return [item for l_1d in l_2d for item in l_1d]


# ~#~#~#~#~#
#  I R P  _  R E L A T E D
# ~#~#~#~#~#
def build_dim(l_dim, colons=False):
    # (List[str],bool) -> str
    '''Contruct a valid fortran90 array dimension code from a list dimension

    Exemple:
	[4,8] -> (4,8) if not colons
	[4,8] -> (:,:) if colons
	
    '''
    if not l_dim:
        return ""

    l_dim_colons = [':'] * len(l_dim) if colons else l_dim

    return "(%s)" % (",".join(l_dim_colons))

def mangled(l_ent, d_ent):
    # (List, Dict[str,Entity]) -> list
    '''Create a uniq list of provider'''
    return OrderedUniqueList(d_ent[name].same_as for name in l_ent)

def build_use(l_ent, d_ent):
    # (List, Dict[str,Entity]) -> list
    '''Contruct the fortran90 'use' statement for the list of entity'''
    return OrderedUniqueList("  use %s" % d_ent[x].fmodule for x in l_ent)

def build_call_provide(l_ent, d_ent):
    # (List, Dict[str,Entity]) -> list
    '''Construct the fortran 90 call the provider needed by the list of entity'''

    # Get the corect name (in the case of multiple provider line)
    l_same_as = mangled(l_ent,d_ent)
    def bld_f90(x):
        return [ "  if (.not.%s_is_built) then" % x,
                 "    call provide_%s" % x,
                 "  endif"]

    return flatten(map(bld_f90, l_same_as))

def che_merge(sets):
    #(List[Set] -> List[Set]
    """Merge a list of set is they are not disjoint.
    Note:
	This will destry sets
    """
    results = []
    upd, isd, pop = set.update, set.isdisjoint, sets.pop
    while sets:
        if not [upd(sets[0],pop(i)) for i in range(len(sets)-1,0,-1) if not isd(sets[0],sets[i])]:
            results.append(pop(0))
    return results


def l_dummy_entity(d_entity):
	from itertools import combinations
	l_candidate_botom = [ (i,j) for i,j in combinations(d_entity.keys(),2) if d_entity[i].children == d_entity[j].children]
	l_dummy = [set([i,j]) for i,j in l_candidate_botom if d_entity[i].parents == d_entity[j].parents]

	return  che_merge(l_dummy)



