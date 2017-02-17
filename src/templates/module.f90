! -*- F90 -*-
!
!-----------------------------------------------!
! This file was generated with the irpf90 tool. !
!                                               !
!           DO NOT MODIFY IT BY HAND            !
!-----------------------------------------------!


MODULE {name}

{#use}
   USE {.}
{/use}

{#usr_declaration}
   {.}
{/usr_declaration}

{#irp_declaration}

 {^dim}
  {type} {?protected}, PROTECTED {/protected} :: {name} {?coarray} [*] {/coarray}
 {:else}

   {?align} !DIR$ ATTRIBUTES ALIGN: {align} :: {name} {/align}
   {type} {?protected}, PROTECTED {/protected} :: {name} {dim} {?coarray} [:] {/coarray}
 {/dim}

 {?main}
  LOGICAL :: {name}_is_built = .FALSE.
 {/main}
{/irp_declaration}

 CONTAINS
     {#irp_declaration}
{protected|s}
     {/irp_declaration}

END MODULE {name}
