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
    use fork_m
    implicit none

    type(Timer) :: stopwatch

    integer(kind=ki4), parameter :: iterations = 1000000_ki4
    integer(kind=ki4) :: i
    real(kind=kr4)    :: elapsedseconds(iterations)
    real(kind=kr4)    :: intervalseconds(iterations)

    ! start it
    call stopwatch%start()

    do i=1,iterations
        call stopwatch%elapsed(elapsedseconds(i))
        call stopwatch%interval(intervalseconds(i))
    enddo

    print *,elapsedseconds(iterations)

    ! end timer
    call stopwatch%stop()

end program example
