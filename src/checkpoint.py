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
from variables import variables
from modules import modules

CHECKPOINT_UNIT_NUMBER=63

FILENAME=irpdir+'irp_checkpoint.irp.F90'

def create():
  out_write      = [ "subroutine irp_checkpoint_write" ] 
  l = variables.keys()
  l.sort
  main_modules = filter(lambda x: modules[x].is_main, modules)
  for m in filter(lambda x: not modules[x].is_main, modules):
    out_write   += [ "  use %s"%(modules[m].name) ]
  out_write     += [ "  implicit none" ]
  out_write     += [ "  integer, parameter :: iunit = %d"%(CHECKPOINT_UNIT_NUMBER) ]
  out_write     += [ "  open(unit=%d,file='irp_checkpoint.dat',status='UNKNOWN',action='WRITE')"%(CHECKPOINT_UNIT_NUMBER) ]
  for v in l:
    var = variables[v]
    if var.is_main:
      out_write     += [ "  if (%s_is_built) then"%(v) ]
      for w in [v]+var.others:
        d = variables[w].dim
        if d == []:
          out_write += [ "    write(iunit,*) '%s', 0"%(w) ]
        else:
          out_write += [ "    write(iunit, *) '%s', %d"%(w, len(d)),  
                        "    write(iunit, *) %s"%(",".join(
                        [ "size(%s,%d)"%(w,i+1) for i in range(len(d)) ] ))
                       ]
        out_write   += [ "    write(iunit,*) %s"%(w) ]
      out_write     += [ "  endif" ]
  out_write         += [ "  close(%d)"%(CHECKPOINT_UNIT_NUMBER) ]
  out_write         += [ "end" ]

  out = '\n'.join(out_write)
  if not same_file(FILENAME,out):
    file = open(FILENAME,'w')
    file.writelines(out)
    file.close()

if __name__ == '__main__':
  create()

