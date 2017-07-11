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

from irpf90_t import irp_id, irpdir
import os
from command_line import command_line


def create(modules, variables):
    # (Dict[str,Module]. Dict[str, Variable]) -> None
    '''Create the fortran90 finalize subroutine and the touched one'''

    main_modules_name = [m.name for m in modules.values() if m.is_main]

    d_template_finalize = {
        'id': irp_id,
        'use': [m.name for m in modules.values() if not m.is_main and m.has_irp_module],
        'entity_array': [{
            'name': e.name,
            'name_root': e.same_as
        } for e in variables.values() if e.fmodule not in main_modules_name and e.dim]
    }

    d_template_touch = {
        'do_debug': command_line.do_debug,
        'entity': [
            e.d_touche_template for e in variables.values()
            if e.fmodule not in main_modules_name and e.d_touche_template
        ]
    }
    import util
    str_out = util.ashes_env.render('touch.f90', d_template_touch) + util.ashes_env.render(
        'finalize.f90', d_template_finalize)

    filename = os.path.join(irpdir, 'irp_touches.irp.F90')
    util.lazy_write_file(filename, '%s\n' % util.remove_empy_lines(str_out))


if __name__ == '__main__':
    create()
