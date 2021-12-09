! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 06 part 01 : 
! Build with:  gfortran -static-libgcc -o aoc_day06_p1 aoc_day06_p1.f90

! Run time on Apple mac Mini M1 (arm64) :  1 minute 7 seconds
! ./aoc_day06_p1  65.02s user 1.87s system 99% cpu 1:06.89 total

program aoc_day06_p1

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 6, part = 1
	
	! Declare file related variables
	character(len=*), parameter :: filename = "day06-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! Variables to track and manage the program
	integer :: total_fish = 0  ! total number of 'fish' numbers in input file
	integer,parameter :: cycle_days = 80      ! number of daily cycles to run through (18 = test / 80 = part 1)
	integer :: c,j,s               ! do loop variables
	integer, allocatable :: fishArray(:)
	
	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1

	call get_fishArray(filename, fishArray, total_fish)

	write(*,'(A,I0,A,I0,A)') " » Total Lantern fish at start: ",total_fish," [check: ",size(fishArray),"]"
	write(*,'(A,I0)') " » Number of daily cycles to model: ",cycle_days
	
	do c=1,cycle_days
		fishArray = fishArray - 1
		s = size(fishArray)  ! capture before we alter it in the loop by appending new fish
		
		do j=1,s
			if (fishArray(j) .EQ. -1 ) then
				fishArray(j) = 6
				fishArray = [fishArray,8]
			end if
		end do
	end do
	
	print *, ""
	write(*,'(A,I0)') " » Part 1 : Total Lantern fish created: ",size(fishArray)
	
	deallocate(fishArray)
	
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
		logical :: exists ! variable for the return.  NB: no intent(out) needed.
				
		inquire(file=filename, exist=exists)
	end function file_exists

    ! Read in the list of Lantern fish from puzzle data source file
    subroutine get_fishArray(filename, fishArray, total_fish)
        character(len=*), intent(in) :: filename
        integer, intent(inout) :: total_fish
        integer, allocatable, intent(inout) :: fishArray(:)
        integer :: fileunit, io, i
        character(len=1) :: A   ! account for ',' separator in in file
 
        open(newunit=fileunit, file=filename, action='read', position='rewind')     
		total_fish = 1  ! account for one more number than commas in file		
		do
			read(fileunit,'(A)',advance="no",iostat=io) A
			if (io /= 0) exit
			if (A .EQ. ",") total_fish = total_fish + 1
        end do
        
        rewind(fileunit)
        
        allocate(fishArray(total_fish))
        read(fileunit,*) fishArray
        
        close(fileunit)
    end subroutine get_fishArray

end program aoc_day06_p1
