! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 11 part 01
! Build with: gfortran -static-libgcc -o aoc_day11_p1 aoc_day11_p1.f90

program aoc_day11_p1

    use, intrinsic :: iso_fortran_env
    implicit none

    ! Declare and set constants the AOC day and part
    integer, parameter :: day = 8, part = 1

    ! Declare file related variables
    character(len = *), parameter :: filename = "day11-input.txt"
    character(len = *), parameter :: error1 = "file: '" // '' // filename // '' // "' not found"

    ! Variables to track and manage the program
    integer :: total_digits = 0

    ! Output startup message
    call display_welcome(day, part)

    if (.NOT. file_exists(filename)) stop error1

    call get_digits(filename, total_digits)

    write(*, '(A,I0)') " » Part 1: Total number of 1,4,7 or 8 digits in output: ", total_digits

    print *, ""

contains

    ! Display the AoC puzzle day and part being resolved
    subroutine display_welcome(day, part)
        implicit none
        integer, intent(in) :: day, part
        print *, ""
        write (*, '(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ", day, " part ", part
        print *, ""
    end subroutine display_welcome

    ! Check input filename exists
    function file_exists(filename) result (exists)
        implicit none
        character(len = *), intent(in) :: filename
        logical :: exists
        inquire(file = filename, exist = exists)
    end function file_exists

    ! Read in the list of output digits from puzzle data source file
    subroutine get_digits(filename, total_digits)
        character(len = *), intent(in) :: filename
        integer, intent(inout) :: total_digits
        integer :: fileunit, io, total_lines, unknown_digits, i
        ! each line of input has:
        ! - 15 total blocks of digits (ascii char a-g) per line
        !   - 1 to 10 blocks : digital input section
        !   - 11 block is ' | '
        !   - 12 to 15 : cover the four digit blocks
        character(len = 9) :: A(15)  ! collect all 15 blocks per line

        open(newunit = fileunit, file = filename, action = 'read', position = 'rewind')

        ! read in each line as 'A' count the "|" per line in puzzle input
        total_lines = 0
        do
            read(fileunit, *, iostat = io) A
            if (io /= 0) exit
            !print *,A
            if (A(11) .EQ. '|') total_lines = total_lines + 1
            ! to view content of each 'A1' to 'A15':
            !do i=1,15
            !	print *, "Output ",i," is: ",A(i)
            !end do
        end do

        write(*, '(A,I0)') " » Total puzzle input lines: ", total_lines

        rewind(fileunit)

        unknown_digits = 0
        total_digits = 0

        do
            read(fileunit, *, iostat = io) A
            if(io .NE. 0) exit
            !print *, len_trim(A(12:15))

            ! scan 'digits' section (A12-A15) for their length - confirms actual digits
            ! displayed as are unique from led segments lit perspective.
            do i = 12, 15
                !print *, len_trim(A(i))  ! <- length gives led segments lit
                select case (len_trim(A(i)))
                case (2)
                    total_digits = total_digits + 1
                    !print *, "Found digit: 1"
                case (3)
                    total_digits = total_digits + 1
                    !print *, "Found digit: 7"
                case (4)
                    total_digits = total_digits + 1
                    !print *, "Found digit: 4"
                case (7)
                    total_digits = total_digits + 1
                    !print *, "Found digit: 8"
                case default
                    unknown_digits = unknown_digits + 1
                    !print *, "Found unknown digits"
                end select

            end do
        end do

        close(fileunit)

    end subroutine get_digits

end program aoc_day11_p1