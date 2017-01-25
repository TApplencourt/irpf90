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

try:
  import irpy
except:
  import lib_irpy as irpy


import getopt, sys
from version import version
import re

description = "IRPF90 Fortran preprocessor."
options = {}
options['a'] = [ 'assert'       , 'Activates ASSERT statements. If absent, remove ASSERT statements.', 0 ]
options['c'] = [ 'codelet'      , 'entity:NMAX or entity:precondition:NMAX  : Generate a codelet to profile a provider running NMAX times', 1 ]
options['C'] = [ 'coarray'      , 'All providers are coarrays', 0 ]
options['d'] = [ 'debug'        , 'Activates debug. The name of the current subroutine/function/provider will be printed on the standard output when entering or exiting a routine, as well as the CPU time passed inside the routine.', 0 ]
options['D'] = [ 'define'       , 'Defines a variable identified by the IRP_IF statements.', 1 ]
options['g'] = [ 'profile'      , 'Activates profiling of the code.', 0 ]
options['h'] = [ 'help'         , 'Print this help', 0 ]
options['I'] = [ 'include'      , 'Include directory', 1 ]
options['j'] = [ 'ninja'        , 'Use Ninja instead of make', 0 ]
options['i'] = [ 'init'         , 'Initialize current directory. Creates a default Makefile and the temporary working directories.', 0 ]
options['l'] = [ 'align'        , 'Align arrays using compiler directives and sets the $IRP_ALIGN variable. For example, --align=32 aligns all arrays on a 32 byte boundary.', 1 ]
options['m'] = [ 'memory'       , 'Print memory allocations/deallocations.', 0 ]
options['n'] = [ 'inline'       , '<all|providers|builders> : Force inlining of providers or builders', 1 ]
options['o'] = [ 'checkopt'     , 'Shows where optimization may be required', 0 ]
options['p'] = [ 'preprocess'   , 'Prints a preprocessed file to standard output. Useful for  debugging files containing shell scripts.', 1 ]
options['r'] = [ 'no_directives', 'Ignore all compiler directives !DEC$ and !DIR$', 0 ]
options['s'] = [ 'substitute'   , 'Substitute values in do loops for generating specific optimized code.', 1 ]
options['t'] = [ 'touch'        , 'Display which entities are touched when touching the variable given as an argument.', 1 ]
options['v'] = [ 'version'      , 'Prints version of irpf90', 0 ]
options['w'] = [ 'warnings'     , 'Activate Warnings', 0 ]
options['z'] = [ 'openmp'       , 'Activate for OpenMP code', 0 ]
options['G'] = [ 'graph'        , 'Print the dependecy-graph of the entities (dots format)', 0 ]

class CommandLine(object):

  def __init__(self):
    global options
    self._opts = None
    self.argv = list(sys.argv)
    self.executable_name = self.argv[0]

  @irpy.lazy_property
  def defined(self):
    return [ a for o,a in self.opts if o in [ "-D", '--'+options['D'][0] ] ]  

  @irpy.lazy_property
  def graph(self):
     return next((a.split() for o,a in self.opts if o in ["-G", '--'+options['G'][0] ]),[])
  
  @irpy.lazy_property
  def include_dir(self):
      self._include_dir = []
      for o,a in self.opts:
        if o in [ "-I", '--'+options['I'][0] ]:
          if len(a) < 1: 
            print "Error: -I option needs a directory"
          if a[-1] != '/':
            a = a+'/'
          self._include_dir.append(a)
      return self._include_dir
  
  @irpy.lazy_property
  def inline(self):
      return next( (a for o,a in self.opts if o in [ "-n", '--'+options['n'][0] ]),'')

  @irpy.lazy_property
  def substituted(self):
      self._substituted = {}
      for o,a in self.opts:
        if o in [ "-s", '--'+options['s'][0] ]:
          k, v = a.split(':')
          v_re = re.compile(r"(\W)(%s)(\W.*$|$)"%k.strip())
          self._substituted[k] = [v, v_re]
      return self._substituted

  @irpy.lazy_property
  def codelet(self):
      for o,a in self.opts:
        if o in [ "-c", '--'+options['c'][0] ]:
          buffer = a.split(':')
          filename = 'codelet_'+buffer[0]+'.irp.f'
          if len(buffer) == 2:
            return  [buffer[0], int(buffer[1]), None, filename]
          elif len(buffer) == 3:
            return [buffer[0], int(buffer[2]), buffer[1], filename]
          else:
            print """
Error in codelet definition. Use:
--codelet=provider:NMAX
or
--codelet=provider:precondition:NMAX
"""
            sys.exit(1)

  @irpy.lazy_property
  def preprocessed(self): 
      return [a for o,a in self.ops if o in [ "-p", '--'+options['p'][0] ] ]

  @irpy.lazy_property
  def touched(self):
       return [a for o,a in self.ops if o in [ "-t", '--'+options['t'][0] ] ]

  @irpy.lazy_property
  def align(self):
    return next( (a for o,a in self.opts if o in [ "-l", '--'+options['l'][0] ]),'1')

  @irpy.lazy_property
  def coarray(self):
    return any(o for o,a in self.opts if o in [ "-C", '--'+options['C'][0] ])

  @irpy.lazy_property
  def warnings(self):
    return any(o for o,a in self.opts if o in [ "-W", '--'+options['W'][0] ]) 

  @irpy.lazy_property
  def openmp(self):
    return any(o for o,a in self.opts if o in [ "-z", '--'+options['z'][0] ])

  @irpy.lazy_property
  def ninja(self):
    return any(o for o,a in self.opts if o in [ "-j", '--'+options['j'][0] ])  

  @irpy.lazy_property
  def directives(self):
    return not(any(o for o,a in self.opts if o in [ "-r", '--'+options['r'][0] ])) 

  def usage(self):
    t = """
$EXE - $DESCR

Usage:
  $EXE [OPTION]

Options:
"""
    t = t.replace("$EXE",self.executable_name)
    t = t.replace("$DESCR",description)
    print t
    print_options()
    print ""
    print "Version : ", version
    print ""

  def opts(self):
    if self._opts is None:
      optlist = ["",[]]
      for o in options:
        b = [o]+options[o]
        if b[3] == 1:
          b[0] = b[0]+":"
          b[1] = b[1]+"="
        optlist[0] += b[0]
        optlist[1] += [b[1]]
    
      try:
        self._opts, args = getopt.getopt(self.argv[1:], optlist[0], optlist[1])
      except getopt.GetoptError, err:
        # print help information and exit:
        self.usage()
        print str(err) # will print something like "option -a not recognized"
        sys.exit(2)
    
    return self._opts
  opts = property(fget=opts)
  
  t = """
def do_$LONG(self):
    if '_do_$LONG' not in self.__dict__:
      self._do_$LONG = False
      for o,a in self.opts:
        if o in ("-$SHORT", "--$LONG"):
          self._do_$LONG = True
          break
    return self._do_$LONG
do_$LONG = property(fget=do_$LONG)
"""
  for short in options:
    long = options[short][0]
    exec t.replace("$LONG",long).replace("$SHORT",short) #in locals()
 
  @irpy.lazy_property 
  def do_run(self):
   return not(any( (self.do_version, self.do_help, self.do_preprocess, self.do_touch, self.do_init)))


command_line = CommandLine()

def print_options():
  keys = options.keys()
  keys.sort()
  import subprocess
  for k in keys:
    description = options[k][1]
    p1 = subprocess.Popen(["fold", "-s", "-w", "40"],stdout=subprocess.PIPE,stdin=subprocess.PIPE)
    description = p1.communicate(description)[0]
    description = description.replace('\n','\n'.ljust(27))
    print ("-%s, --%s"%(k,options[k][0])).ljust(25), description+'\n'
  print "\n"

if __name__ == '__main__':
  print_options()
