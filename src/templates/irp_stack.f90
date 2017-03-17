MODULE irp_stack_mod
  INTEGER, PARAMETER             :: STACKMAX=1000
  CHARACTER*(128)  ,allocatable  :: irp_stack(:,:)
  DOUBLE PRECISION ,allocatable  :: irp_cpu(:,:)
  INTEGER          ,allocatable  :: stack_index(:)
  INTEGER                        :: nthread
  CHARACTER*(128)                :: white = ''
END MODULE

SUBROUTINE irp_stack_init
    USE irp_stack_mod

    IMPLICIT NONE

    INTEGER                    :: ithread
    {?do_openmp}
    INTEGER, EXTERNAL :: omp_get_thread_num
    INTEGER, EXTERNAL :: omp_get_max_threads
    {/do_openmp}
    INTEGER           :: ierr
    {^do_openmp}
    ithread = 0
    {:else}
    ithread = omp_get_thread_num()
    {/do_openmp}

    {^do_openmp} !$OMP CRITICAL {/do_openmp}
    IF (.NOT.ALLOCATED(stack_index) ) THEN

      {^do_openmp}
      nthread = 1
      {:else}
       nthread = omp_get_max_threads()
      {/do_openmp}

      {?do_memory}
       print *, 'Allocating irp_stack(0:',STACKMAX,',0:',nthread,')'
       print *, 'Allocating irp_cpu(0:',STACKMAX,',0:',nthread,')'
       print *, 'Allocating stack_index(0:',nthread,')'
      {/do_memory}

       ALLOCATE ( irp_stack(0:STACKMAX, 0:nthread), &
                  irp_cpu(0:STACKMAX, 0:nthread),   &
                  stack_index(0:nthread)  )
       IF (ierr /=0 ) THEN
          print*, 'Failed Allocating irp_stack, irp_cpu, stack_index'
       ENDIF
       stack_index = 0
    END IF

END SUBROUTINE

SUBROUTINE irp_enter_routine(irp_where)
    USE irp_stack_mod

    IMPLICIT NONE

    CHARACTER*(*), INTENT(in)  :: irp_where
    INTEGER                    :: ithread
    REAL                       :: cpu
    {?do_openmp}
    INTEGER, EXTERNAL :: omp_get_thread_num
    {/do_openmp}

    {^do_openmp}
    ithread = 0
    {:else}
    ithread = omp_get_thread_num()
    {/do_openmp}

    stack_index(ithread) = min(stack_index(ithread)+1,STACKMAX)
    irp_stack(stack_index(ithread),ithread) = irp_where

END SUBROUTINE irp_enter_routine

SUBROUTINE irp_enter(irp_where)
    USE irp_stack_mod

    IMPLICIT NONE

    CHARACTER*(*), INTENT(in)  :: irp_where
    INTEGER                    :: ithread
    {?do_openmp}
    INTEGER, EXTERNAL :: omp_get_thread_num
    {/do_openmp}

    {^do_openmp}
    ithread = 0
    {:else}
    ithread = omp_get_thread_num()
    {/do_openmp}

    print *, ithread, ':', white(1:stack_index(ithread))//'-> ', trim(irp_where)
    CALL cpu_time(irp_cpu(stack_index(ithread),ithread))
END SUBROUTINE irp_enter


SUBROUTINE irp_leave(irp_where)
    USE irp_stack_mod

    IMPLICIT NONE

    CHARACTER*(*), INTENT(in)  :: irp_where
    INTEGER                    :: ithread
    REAL                       :: cpu
    {?do_openmp}
    INTEGER, EXTERNAL :: omp_get_thread_num
    {/do_openmp}

    {^do_openmp}
    ithread = 0
    {:else}
    ithread = omp_get_thread_num()
    {/do_openmp}

    CALL cpu_time(cpu)
    print *, ithread, ':', white(1:stack_index(ithread))//'<- ', &
                           trim(irp_stack(stack_index(ithread),ithread)), &
                           cpu - irp_cpu(stack_index(ithread),ithread)

    stack_index(ithread) = max(0,stack_index(ithread)-1)

END SUBROUTINE irp_leave


SUBROUTINE irp_trace
    USE irp_stack_mod

    IMPLICIT NONE

    INTEGER                    :: ithread
    {?do_openmp}
    INTEGER, EXTERNAL :: omp_get_thread_num
    {/do_openmp}
    INTEGER                  :: i

    {^do_openmp}
    ithread = 0
    {:else}
    ithread = omp_get_thread_num()
    {/do_openmp}

    print *, 'Stack trace: ', ithread
    print *, '-------------------------'
    DO i=1,stack_index(ithread)
        print *, trim(irp_stack(i,ithread))
    END DO
    print *, '-------------------------'

END SUBROUTINE irp_trace

