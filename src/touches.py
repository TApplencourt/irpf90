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

from irpf90_t import irp_id,irpdir
import os
from util import lazy_write_file

def create(modules,variables):
  # (Dict[str,Module]. Dict[str, Variable]) -> None
  '''Create the fortran90 finalize subroutine and the touched one'''

  
  finalize = "subroutine irp_finalize_%s\n"%(irp_id)
  for m in filter(lambda x: not modules[x].is_main and modules[x].has_irp_module, modules):
      finalize += " use %s\n"%(modules[m].name)

  main_modules_name =[ m.name for m in modules.values() if m.is_main]
  
  out = []
  for v,var in variables.iteritems():

    if var.fmodule not in main_modules_name:
      out += var.toucher
      if var.dim:
        finalize += "  if (allocated(%s)) then\n"%v
        finalize += "    %s_is_built = .False.\n"%var.same_as
        finalize += "    deallocate(%s)\n"%v
        finalize += "  endif\n"

  finalize += "end\n"

  if out:
    out = map(lambda x: "%s\n"%(x),out)

  out += finalize

  filename=os.path.join(irpdir,'irp_touches.irp.F90')
  lazy_write_file(filename,''.join(out))

if __name__ == '__main__':
  create()
