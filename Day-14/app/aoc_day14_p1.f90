! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 14 part 01 :
! Build (or run) with:
! gfortran -static-libgcc -o aoc_day14_p1 aoc_day14_p1.f90
! fpm run aoc_day14_p1

program aoc_day14_p1

    use, intrinsic :: iso_fortran_env, stderr => error_unit
    implicit none

    ! Declare and set constants the AOC day and part
    integer, parameter :: day = 14, part = 1

    ! Declare file related variables
    ! - Prod uses: day14-input.txt
    ! - Test uses: day14-TEST-input.txt
    character(len = *), parameter :: filename = "data/day14-input.txt"

    ! Variables to track and manage the program
    integer :: total_lines = 0
    integer :: i,j,n,m
    character(len=100) :: poly_temp


    ! Output startup message
    call display_welcome(day, part)

    print *, ""
    print *, "INCOMPLETE - work in progress still..."
    print *, ""

    ! Check input puzzle data file and get its line count
    total_lines = get_line_count(filename)
    if (total_lines == -1) stop ": Input file not found."
    write (*, '(A,A,A,I3.3)') "Filename: '", filename, "' line count is: ", total_lines


contains

    ! Display the AoC puzzle day and part being resolved
    subroutine display_welcome(day, part)
        implicit none
        integer, intent(in) :: day, part
        print *, ""
        write (*, '(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ", day, " part ", part
        print *, ""
    end subroutine display_welcome

    ! If the file exists return its total line count or '-1' on error
    ! TODO: add optional flag to run 'stop' instead of returning '-1' if needs to be a fatal error?
    function get_line_count(filename) result (lines)
        implicit none
        character(len = *), intent(in) :: filename
        integer :: fileunit, io
        integer :: lines
        logical :: exists

        inquire(file = filename, exist = exists)
        if (.not. exists) then
            ! below should be printed to stderr!!
            write (stderr, '(A,A)') "ERROR: unable to locate file name: ", filename
            lines = -1
            return
        end if

        open(newunit = fileunit, file = filename, action = 'read', position = 'rewind')
        lines = 0
        do
            read(fileunit, *, iostat = io)
            if (io /= 0) exit
            lines = lines + 1
        end do

        close(fileunit)
    end function get_line_count

end program aoc_day14_p1