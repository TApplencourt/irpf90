{#entity}
SUBROUTINE irp_lock_{.}(set)

   USE omp_lib
   IMPLICIT NONE
   LOGICAL, INTENT(in) :: set
   INTEGER(KIND=omp_lock_kind),SAVE :: {.}_lock
   INTEGER, SAVE       :: ifirst = 0

 {?do_debug}
   CHARACTER*(9+{@size key={.}/}),PARAMETER :: irp_here = 'irp_lock_{name}'
 {/do_debug}
 
 {?do_debug} CALL irp_enter(irp_here) {/do_debug}

  IF (ifirst == 0) then
       ifirst = 1
       CALL omp_init_lock({.}_lock)
  ENDIF

  IF (set) THEN
       CALL omp_set_lock({.}_lock)
  ELSE
       CALL omp_unset_lock({.}_lock)
  ENDIF
  
 {?do_debug} CALL irp_leach(irp_here) {/do_debug}

END SUBROUTINE irp_lock_{.}
{/entity}

