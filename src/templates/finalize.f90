SUBROUTINE irp_finalize_{id}
   {#use}
   USE {.}
   {/use} 

   IMPLICIT NONE
   {#entity_array}
   IF (ALLOCATED({name})) THEN
      {name_root}_is_built = .FALSE.
!      DEALLOCATE({name})
   ENDIF
   {/entity_array}
END SUBROUTINE irp_finalize_{id}
