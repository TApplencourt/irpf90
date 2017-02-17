SUBROUTINE write_{name}(irp_num)

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

    {children}
    CALL write_{.}(irp_num)
    {/children}

    DO WHILE (irp_is_open)
          irp_iunit = irp_inuit + 1
          INQUIRE(UNIT=irp_inuit, OPENED=irp_is_open)
    END DO

    {#group_entity}
       OPEN(UNIT=irp_inuit,file='irpf90_{name}_'//trim(irp_num),FROM='FORMATTED',STATUS='UNKNOWN',ACTION='WRITE')
       WRITE(irp_inuit,*) {.}{dim}
       CLOSE(irp_inuit)
    {/group_entity}

   {?do_debug} CALL irp_leave(irp_here) {/do_debug}

END SUBROUTINE write_{name}

SUBROUTINE read_{name}(irp_num)

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
          irp_iunit = irp_inuit + 1
          INQUIRE(UNIT=irp_inuit, OPENED=irp_is_open)
    END DO

    {#group_entity}
       OPEN(UNIT=irp_inuit,file='irpf90_{name}_'//trim(irp_num),FROM='FORMATTED',STATUS='UNKNOWN',ACTION='WRITE')
       READ(irp_inuit,*) {name}{dim}
       CLOSE(irp_inuit)
    {/group_entity}
   
   CALL touch_{name}
   {?do_debug} CALL irp_leave(irp_here) {/do_debug}

END SUBROUTINE read_{name}

