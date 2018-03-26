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

#ifndef PR
#define PR sp
#define _PR _sp
#endif

program fortimexample
    use kinds_m
    use fortime_m
    implicit none

    type(Timer) :: stopwatch
    integer(kind=sp), parameter :: iterations = 10_sp

    ! start
    call stopwatch%start()

    ! runs
    call run(10_PR)
    call run(100_PR)
    call run(500_PR)
    call run(1000_PR)
    call run(5000_PR)
    call run(10000_PR)
    call run(20000_PR)

    ! end
    call stopwatch%finish()

contains

    !> run some iterations and get the average time taken
    subroutine run(sze)
        integer(kind=PR), intent(in) :: sze

        integer(kind=PR) :: k
        real(kind=sp)    :: averagetime

        averagetime = 0.0_PR
        do k=1,iterations
            averagetime = averagetime + matrixcreate(sze)
        enddo
        averagetime = averagetime/iterations
        print *, "Average time taken for matrix of size:", sze, "is:", averagetime, "secs"
    end subroutine run

    !> Create a matrix and get the time taken
    function matrixcreate(sze) result(timetaken)
        integer(kind=PR), intent(in) :: sze
        real(kind=sp) :: timetaken, elapsedtime

        real(kind=PR), dimension(:,:), allocatable :: mat
        integer(kind=sp) :: stat

        allocate(mat(sze,sze),stat=stat)
        if(stat.ne.0)then
            stop "Cannot allocate memory"
        endif
        call dowork(mat,sze)
        deallocate(mat)

        call stopwatch%elapsed(elapsedtime)
        call stopwatch%interval(timetaken)

    end function matrixcreate

    !> Do some silly work
    subroutine dowork(matrix, sze)
        real(kind=PR), dimension(:,:) :: matrix
        integer(kind=PR), intent(in) :: sze

        integer(kind=PR) :: i,j

        !> do silly work
        do i=1, sze
            do j=1, sze
                matrix(i,j) = i*j/3.14
            enddo
        enddo

    end subroutine dowork

end program fortimexample
