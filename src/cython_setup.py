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

from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import os

to_remove = """cython_setup.py version.py command_line.py""".split()
ext_modules = []

files = os.listdir('.')
for file in to_remove:
  files.remove(file)

for file in files:
  if file.endswith(".py"):
   module = file.split('.')[0]
   ext_modules += [ Extension(module,[file]) ]

setup(
  name = 'IRPF90 extensions',
  cmdclass = {'build_ext': build_ext},
  ext_modules = ext_modules
)




