BEGIN_PROVIDER [integer, t ]
  IMPLICIT NONE
  t = u1+v+4
END_PROVIDER

BEGIN_PROVIDER [integer,w]
  IMPLICIT NONE
  w = d5+3
END_PROVIDER

BEGIN_PROVIDER [ integer, v ]
 IMPLICIT NONE
  v = u2+w+2
END_PROVIDER

BEGIN_PROVIDER [ integer, u1 ]
  IMPLICIT NONE
  integer :: fu
  u1 = fu(d1,d2)
END_PROVIDER

BEGIN_PROVIDER [ integer, u2 ]
  IMPLICIT NONE
  integer :: fu
  u2 = fu(d3,d4)
END_PROVIDER

INTEGER function fu(x,y)
  INTEGER, INTENT(in) :: x,y
  fu = int(x+y+1+3)
END FUNCTION
