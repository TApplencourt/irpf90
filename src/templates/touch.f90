{#entity}

SUBROUTINE touch_{name}

 {#l_module}
   USE {.}
 {/l_module}

   IMPLICIT NONE
  {?do_debug}
   CHARACTER*(6+{@size key=name/}),PARAMETER :: irp_here = 'touch_{name}'
  {/do_debug}

 {?do_debug} CALL irp_enter(irp_here) {/do_debug}

 {#l_ancestor}
   {.}_is_built = .FALSE.
 {/l_ancestor}

   {name}_is_built = .TRUE.

 {?do_debug} CALL irp_leave(irp_here) {/do_debug}

END SUBROUTINE touch_{name}

{/entity}
