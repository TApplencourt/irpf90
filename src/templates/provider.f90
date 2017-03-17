{?inline}!DEC$ ATTRIBUTES FORCEINLINE :: provide_{name}{/inline}
SUBROUTINE provide_{name}

 {#l_module}
  USE {.}
 {/l_module}

  IMPLICIT NONE

 {?do_debug}
  CHARACTER*(8+{@size key=name/}),PARAMETER :: irp_here = 'provide_{name}'
 {/do_debug}

 {?do_debug} CALL irp_enter(irp_here) {/do_debug}

 {?do_openmp} 
 CALL irp_lock_{name}(.TRUE.) 
 IF (.NOT.{name}_is_built) THEN
 {/do_openmp}

   {#l_children_static}
   {@first} {?do_task}!$OMP TASKGROUP{/do_task} {/first}
   {?do_openmp}!$OMP flush({.}_is_built){/do_openmp}
   IF (.NOT.{.}_is_built) THEN
      {?do_task}!$OMP TASK{/do_task}
      CALL provide_{.}
      {?do_task}!$OMP END TASK{/do_task}
   ENDIF
   {@last} {?do_task}!$OMP END TASKGROUP{/do_task} {/last}
   {/l_children_static}

   {#l_entity}
   {?dim} CALL allocate_{name} {/dim}
   {/l_entity}

   CALL bld_{name}
   {name}_is_built = .TRUE.

 {?do_openmp}
 ENDIF
 CALL irp_lock_{name}(.FALSE.)
 {/do_openmp}
 
 {?do_debug} CALL irp_leave(irp_here) {/do_debug}
END SUBROUTINE provide_{name}
