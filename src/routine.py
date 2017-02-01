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

from util import logger

try:
  import irpy
except:
  import lib_irpy as irpy

class Routine(object):  
  '''
  A collection of list corresponding of a Routine (Subroutine, or function)
  '''


  ############################################################
  def __init__(self,text):
    assert type(text) == list
    assert len(text) > 0

    self.text = text
    self.prototype = self.text[0]
    assert isinstance(self.prototype, (Subroutine, Function))

  ############################################################
  @irpy.lazy_property_mutable
  def called_by(self):
    raise AttributeError
 
  ############################################################
  @irpy.lazy_property
  def name(self):
    '''Name is lowercase'''
    return self.prototype.subname

  ############################################################
  @irpy.lazy_property
  def is_function(self):
      return "function" in self.prototype.lower

  ############################################################
  @irpy.lazy_property
  def is_subroutine(self):
      return "subroutine" in self.prototype.lower

  ############################################################
  @irpy.lazy_property
  def doc(self):

      l_doc = [ l for l in  self.text if isinstance(l,Doc) ]
      if not l_doc:
        logger.info("Subroutine '%s' is not documented"%(self.name))
      return [l.text.lstrip()[1:] for l in l_doc]

  ############################################################
  @irpy.lazy_property
  def touches_my_self(self):
        return set(x for line in self.text for x in line.text.split()[1:] if isinstance(line,(Touch, SoftTouch)))

  @irpy.lazy_property_mutable
  def touches_ancestor(self):
        raise AttributeError

  @irpy.lazy_property
  def touches(self):
        return list(self.touches_my_self.union(self.touches_ancestor))

  ############################################################
  @irpy.lazy_property
  def regexp(self):
      import re
      return re.compile(r"([^a-z0-9'\"_]|^)%s([^a-z0-9_]|$)"%(self.name),re.I)

  ############################################################
  @irpy.lazy_property
  def calls(self):
      return set(line.text.split('(',1)[0].split()[1].lower() for line in self.text if isinstance(line,Call))
