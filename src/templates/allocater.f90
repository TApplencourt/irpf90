{?dim}
SUBROUTINE  allocate_{name}

 {#l_module}
  USE {.}
 {/l_module}

  IMPLICIT NONE

  CHARACTER*(9+{@size key=name/}),PARAMETER :: irp_here = 'allocate_{name}'
  INTEGER :: irp_err
  LOGICAL :: alloc

  alloc = ALLOCATED({name})

  IF ( alloc .AND.( &
     {#l_dim}
     ( SIZE({name},{rank}) == {value} ) {@sep}.OR.{/sep} &
     {/l_dim})) THEN

     RETURN
  ELSE IF (.NOT.alloc) THEN
      GO TO 666
  ELSE 
     {?do_memory} PRINT*, irp_here//': Deallocated {name}' {/do_memory}
     DEALLOCATE({name},STAT=irp_err)

     IF (irp_err /= 0) THEN
          PRINT*, irp_here//': Deallocation failed: {name}'
          PRINT*,' size: {dim}'
     ENDIF

     GO TO 666
  ENDIF

  666 CONTINUE
  {?do_memory} PRINT*, irp_here//': Allocate {name} ({dim})'{/do_memory}

  {^do_corray}
  ALLOCATE({name} ({dim}),    STAT=irp_err)
  {:else}
  ALLOCATE({name} ({dim}[*]), STAT=irp_err)
  {/do_corray}
  
  IF (irp_err /= 0) then
      PRINT*, irp_here//': Allocation failed: {name}'
      PRINT*,' size: {dim}'
  ENDIF
END SUBROUTINE allocate_{name}
{/dim}
