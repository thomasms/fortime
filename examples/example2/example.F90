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

program fortimexample
    use fork_m
    use fortime_m
    implicit none

    type(Timer) :: stopwatch
    integer(kind=ki4), parameter :: iterations = 10_ki4

    ! start
    call stopwatch%start()

    ! runs
    call run(10_ki4)
    call run(100_ki4)
    call run(500_ki4)
    call run(1000_ki4)
    call run(5000_ki4)
    call run(10000_ki4)
    call run(20000_ki4)

    ! end
    call stopwatch%stop()

contains

    !> run some iterations and get the average time taken
    subroutine run(sze)
        integer(kind=ki4), intent(in) :: sze

        integer(kind=ki4) :: k
        real(kind=kr4)   :: averagetime

        averagetime = 0.0_kr4
        do k=1_ki4, iterations
            averagetime = averagetime + matrixcreate(sze)
        end do
        
        averagetime = averagetime/iterations
        print *, "Average time taken for matrix of size:", sze, "is:", averagetime, "secs"
    end subroutine run

    !> Create a matrix and get the time taken
    function matrixcreate(sze) result(timetaken)
        integer(kind=ki4), intent(in) :: sze
        real(kind=kr4) :: timetaken, elapsedtime

        real(kind=kr4), dimension(:,:), allocatable :: mat
        integer(kind=ki4) :: stat

        allocate(mat(sze,sze), stat=stat)
        if(stat /= 0)then
            stop "Cannot allocate memory"
        end if

        call dowork(mat,sze)
        deallocate(mat)

        call stopwatch%elapsed(elapsedtime)
        call stopwatch%interval(timetaken)

    end function matrixcreate

    !> Do some silly work
    subroutine dowork(matrix, sze)
        real(kind=kr4), dimension(:,:) :: matrix
        integer(kind=ki4), intent(in) :: sze

        integer(kind=ki4) :: i, j

        !> do silly work
        !! Fortran is column major
        do j = 1_ki4, sze
            do i = 1_ki4, sze
                matrix(i,j) = i*j/3.14
            enddo
        enddo

    end subroutine dowork

end program fortimexample
