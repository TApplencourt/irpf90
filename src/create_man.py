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

from entity import Entity
from routine import Routine
from irpf90_t import mandir
from util import parmap, build_dim, lazy_write_file
import os


def do_print_short(entity):
    assert type(entity) == Entity
    str_ = "{0:<35} : {1:<30} :: {2:<25}  {3}".format(entity.prototype.filename[0],
                                                      entity.type,
                                                      entity.name,
                                                      build_dim(entity.dim)) # yapf: disable
    return [str_]


######################################################################
def process_doc(line):
    assert type(line) == str
    line = line.strip()
    return [line if line else ".br"]


######################################################################
def process_deps(l):
    assert isinstance(l, (list, set))
    return ['%s\n.br' % v for v in sorted(l)]


######################################################################
def process_types(entity_input, d_entity):
    assert type(entity_input) == Entity

    l_name = entity_input.l_name
    l_entity = [d_entity[name] for name in l_name]

    l = [ "{0}\t:: {1}\t{2}".format(entity.type, name, build_dim(entity.dim) )
          for name,entity in zip(l_name,l_entity) ] # yapf: disable

    return l


######################################################################
def do_print(entity, d_entity):
    assert type(entity) == Entity
    filename = entity.prototype.filename[0]
    name = entity.name

    l_data = []

    l_data.append('.TH "IRPF90 entities" l {0} "IRPF90 entities" {0}'.format(name))

    if entity.same_as != entity.name:
        entity = d_entity[entity.same_as]
    l_data.extend([".SH Declaration", ".nf"])
    l_data += process_types(entity, d_entity)

    l_data.append(".ni")

    if entity.doc:
        l_data.append(".SH Description")
        for l in entity.doc:
            l_data += process_doc(l)

    l_data.append(".SH File\n.P")
    l_data.append(filename)

    if entity.needs:
        l_data.append(".SH Needs")
        l_data += process_deps(entity.needs)

    if entity.needed_by:
        l_data.append(".SH Needed by")
        l_data += process_deps(entity.needed_by)

    l_data.append(".SH Instability factor")
    fo = len(entity.children)
    fi = len(entity.parents)
    l_data.append("%5.1f %%" % (100. * (fi / (fi + fo + .000001))))
    l_data.append(".br")
    str_ = '\n'.join(l_data)
    lazy_write_file("%s%s.l" % (mandir, name), '%s\n' % str_)


######################################################################
def do_print_subroutines(sub):
    assert type(sub) == Routine
    filename = sub.prototype.filename
    name = sub.name
    l_data = []

    l_data.append('.TH "IRPF90 entities" l {0} "IRPF90 entities" {0}'.format(name))
    l_data.append(".SH Declaration")
    l_data.append(".nf")
    l_data += [sub.prototype.text]
    l_data.append(".ni")
    if sub.doc:
        l_data.append(".SH Description")
        for l in sub.doc:
            l_data += process_doc(l)
    l_data.append(".SH File\n.P")

    l_data.append(filename)
    if sub.needs:
        l_data.append(".SH Needs")
        l_data += process_deps(sub.needs)

    if sub.called_by:
        l_data.append(".SH Called by")
        l_data += process_deps(sub.called_by)

    if sub.calls:
        l_data.append(".SH Calls")
        l_data += process_deps(sub.calls)

    if sub.touches:
        l_data.append(".SH Touches")
        l_data += process_deps(sub.touches)

    l_data.append(".SH Instability factor")
    fo = len(sub.needs) + len(sub.calls) + len(sub.touches)
    fi = len(sub.called_by)
    l_data.append("%5.1f %%" % (100. * (fi / (fi + fo + .000001))))
    l_data.append(".br")

    str_ = '\n'.join(l_data)

    return '%s\n' % str_

    #lazy_write_file(filename="%s.l" % os.path.join(mandir, name), text='%s\n' % str_)


######################################################################
def run(d_entity, d_routine):

    for v in d_entity.values():
        do_print(v, d_entity)

    l_subs = d_routine.values()

    l_data_to_write = [("%s.l" % os.path.join(mandir, s.name), do_print_subroutines(s))
                       for s in l_subs]

    def worker(l):
        filename, text = l
        lazy_write_file(filename, text)

    parmap(worker, l_data_to_write)

    tags = []

    for v in d_entity.keys():
        line = d_entity[v].prototype
        tags.append('%s\t%s\t%d\n' % (v, line.filename[0], line.i))

    for v in d_routine.keys():
        line = d_routine[v].prototype
        tags.append('%s\t%s\t%d\n' % (v, line.filename, line.i))

    lazy_write_file("tags", ''.join(sorted(tags)))

    l_data = [e for k, v in sorted(d_entity.items()) for e in do_print_short(v)]
    str_ = '\n'.join(l_data)
    lazy_write_file(filename="irpf90_entities", text='%s\n' % str_)

######################################################################
if __name__ == '__main__':
    run()
