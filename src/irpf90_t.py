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


irpdir = "IRPF90_temp/"
mandir = "IRPF90_man/"

from zlib import crc32
import os
irp_id = abs(crc32(os.getcwd()))

try:
  import irpy
except:
  import lib_irpy as irpy


class Line(object):
    def __init__(self, i, text, filename):
        self.i = i
        self.text = text
        self.filename = filename

    @irpy.lazy_property
    def lower(self):
        return self.text.lower()

    def __repr__(self):
        return "%20s:%5d : %s (%s)" % (type(self).__name__, self.i, self.text, self.filename)

class LineWithName(Line):

  @irpy.lazy_property
  def subname(self):
       buf = self.lower
       if not buf.endswith(')'):
         buf += "()"

       l_buf = buf.split('(')
       l_name = l_buf[0].split()

       if len(l_name) < 2:
                import loger 
                logger.error("Syntax Error: %s" % line)
                sys.exit(1)
       return l_name.pop()

l_type = [
    'Empty_line', 'Simple_line', "Declaration", "Continue", "Begin_provider",
    "Cont_provider", "End_provider", "Begin_doc", "Doc", "End_doc",
    "Begin_shell", "End_shell", "Begin_template", "End_template", "Subst",
    "Assert", "Touch", "SoftTouch", "Irp_read", "Irp_write", "Irp_If",
    "Irp_Else", "Irp_Endif", "Openmp", "Directive", "Use", "Do", "Enddo", "If",
    "Elseif", "Else", "Endif", "Select", "Case", "End_select", "Provide", "NoDep", "Return", "Include",
    "Implicit", "Free", "End", "Provide_all","Contains",'Type','End_module','Interface','End_interface',
    'Where','Elsewhere','Endwhere']

for t in l_type:
    globals()[t] = type(t, (Line, ), {})

for t in ['Subroutine', 'Function', 'Program',  'Call','Module']:
     globals()[t] = type(t, (LineWithName, ), {}) 

