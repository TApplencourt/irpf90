SUBROUTINE writer_{name}(irp_num)

    USE {fmodule} 
    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: irp_num
    LOGICAL                   :: irp_is_open = .TRUE.
    INTEGER                   :: irp_iunit = 9

 {?do_debug}
    CHARACTER*(7+{@size key=name/}),PARAMETER :: irp_here = 'writer_{name}'
 {/do_debug}

   {?do_debug} CALL irp_enter(irp_here) {/do_debug}

    IF (.NOT.{same_as}_is_built) THEN
         CALL provide_{same_as}
    ENDIF

    {#children}
    CALL writer_{.}(irp_num)
    {/children}

    DO WHILE (irp_is_open)
          irp_iunit = irp_iunit + 1
          INQUIRE(UNIT=irp_iunit, OPENED=irp_is_open)
    END DO

    {#group_entity}
       OPEN(UNIT=irp_iunit,file='irpf90_{name}_'//trim(irp_num),FORM='FORMATTED',STATUS='UNKNOWN',ACTION='WRITE')
       WRITE(irp_iunit,*) {name}{dim}
       CLOSE(irp_iunit)
    {/group_entity}

   {?do_debug} CALL irp_leave(irp_here) {/do_debug}

END SUBROUTINE writer_{name}

!TOKEN_SPLIT

SUBROUTINE reader_{name}(irp_num)

    USE {fmodule}
    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: irp_num
    LOGICAL                   :: irp_is_open = .TRUE.
    INTEGER                   :: irp_iunit = 9

 {?do_debug}
    CHARACTER*(5+{@size key=name/}),PARAMETER :: irp_here = 'read_{name}'
 {/do_debug}

   {?do_debug} CALL irp_enter(irp_here) {/do_debug}

    DO WHILE (irp_is_open)
          irp_iunit = irp_iunit + 1
          INQUIRE(UNIT=irp_iunit, OPENED=irp_is_open)
    END DO

    {#group_entity}
       OPEN(UNIT=irp_iunit,file='irpf90_{name}_'//trim(irp_num),FORM='FORMATTED',STATUS='OLD',ACTION='READ')
       READ(irp_iunit,*) {name}{dim}
       CLOSE(irp_iunit)
    {/group_entity}
   
   CALL touch_{same_as}
   {?do_debug} CALL irp_leave(irp_here) {/do_debug}

END SUBROUTINE reader_{name}

