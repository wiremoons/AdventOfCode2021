! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 07 part 01 : 
! Build with:  gfortran -static-libgcc -o aoc_day07_p1 aoc_day07_p1.f90

program aoc_day07_p1

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 7, part = 1
	
	! Declare file related variables
	character(len=*), parameter :: filename = "day07-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! Variables to track and manage the program
	integer :: total_crabs = 0
	integer :: total_positions = 0
	integer, allocatable :: crabArray(:)
	integer :: i
	
	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1
	
	call get_crabs(filename, total_crabs, crabArray)
	
	total_positions = dim((maxval(crabArray)),(minval(crabArray)))
	if (minval(crabArray) .eq. 0) total_positions = total_positions + 1

	write(*,'(A,I0)') " » Total crabs in array: ",size(crabArray)
	write(*,'(A,I0)') " » Min crab position: ",minval(crabArray)
	write(*,'(A,I0)') " » Max crab position: ",maxval(crabArray)
	write(*,'(A,I0)') " » Number of potential postions: ",total_positions
	
	print *, ""
	write(*,'(A,I0)') " » Part 1: Fuel used is: ",minval((/(sum(abs(crabArray-i)),i=0,maxval(crabArray))/))
	print *, ""

contains

	! Display the AoC puzzle day and part being resolved  
	subroutine display_welcome(day,part)
		implicit none
		integer, intent(in) :: day,part
		print *,""
		write (*,'(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ",day," part ",part
		print *,""
	end subroutine display_welcome

	! Check input filename exists 
	function file_exists(filename) result (exists)
		implicit none
		character(len=*), intent(in) :: filename
		logical :: exists
		inquire(file=filename, exist=exists)
	end function file_exists

    ! Read in the list of crabs positions from puzzle data source file
    subroutine get_crabs(filename, total_crabs, crabArray)
        character(len=*), intent(in) :: filename
        integer, intent(inout) :: total_crabs
        integer, allocatable, intent(inout) :: crabArray(:)
        integer :: fileunit, io
        character(len=1) :: A   ! account for ',' separator in in file
 
        open(newunit=fileunit, file=filename, action='read', position='rewind')
        
        total_crabs = 1  ! account for one more number than commas in file       
        
        do
            read(fileunit,'(A)',advance="no",iostat=io) A
            if (io /= 0) exit
            if (A .EQ. ",") total_crabs = total_crabs + 1
        end do
        
        !print *, "File read total crab count: ",total_crabs

		rewind(fileunit)
		allocate(crabArray(total_crabs))

		read(fileunit,*) crabArray
        
        close(fileunit)
        
    end subroutine get_crabs

end program aoc_day07_p1