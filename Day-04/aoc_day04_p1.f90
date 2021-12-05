! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 04 part 01 : 
! Build with:  gfortran -static-libgcc -o aoc_day04_p1 aoc_day04_p1.f90

program aoc_day04_p1

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 4, part = 1
	
	! Declare file related variables
	character(len=*), parameter :: filename = "day04-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! General program variables
	integer :: total_call_nums = 0
	integer, allocatable :: numbersArray(:) ! imported bingo call numbers array
	integer :: board(5,5) ! bingo board array

	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1

	call get_bingo_call_numbers(filename,numbersArray,total_call_nums)
	
	write(*,'(A,I3.3,A,I3.3,A)') " » Bingo calls to be made: ",total_call_nums," [array check: ",size(numbersArray),"]"
	!write(*,'(I2.2)') numbersArray ! print all bingo call numbers extracted from input
		
	! clean up arrays allocated
	deallocate(numbersArray)
	
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
		logical :: exists ! variable for the return.  NB: no intent(out) needed.
				
		inquire(file=filename, exist=exists)
	end function file_exists

	! Check how many numbers exists for the bingo calls - first input line of numbers
	! Read all numbers extracted into the array 'numbersArray' 
	subroutine get_bingo_call_numbers (filename,numbersArray,call_nums)
		character(len=*), intent(in) :: filename
		integer, allocatable,intent(inout) :: numbersArray(:)
		integer :: fileunit,io
		character(len=1) :: A
		integer, intent(inout) :: call_nums

		open(newunit=fileunit, file=filename, action='read', position='rewind')
		call_nums = 1  ! one more number that commas - so start at 1 not 0
		do
			read(fileunit,'(A)',advance='no',iostat=io) A
			if (io /= 0) exit
			if (A .EQ. ",") call_nums = call_nums + 1
		end do
		
		rewind(fileunit)

		allocate(numbersArray(call_nums))
		read(fileunit,*) numbersArray
		close(fileunit)
		
	end subroutine get_bingo_call_numbers

end program aoc_day04_p1
