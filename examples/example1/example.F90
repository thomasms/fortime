!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                  FORTIME                      !!
!!                                               !!
!!      Copyright (c) 2018, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program example
    use fortime_m
    use kinds_m
    implicit none

    type(Timer) :: stopwatch

    integer(kind=dp), parameter :: iterations = 1000000_dp
    integer(kind=dp) :: i
    real(kind=sp)    :: elapsedseconds(iterations)
    real(kind=sp)    :: intervalseconds(iterations)

    ! start it
    call stopwatch%start()

    do i=1,iterations
        call stopwatch%elapsed(elapsedseconds(i))
        call stopwatch%interval(intervalseconds(i))
    enddo

    print *,elapsedseconds(iterations)

    ! end timer
    call stopwatch%finish()

end program example
