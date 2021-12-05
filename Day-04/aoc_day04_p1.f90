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

	! Variables to track and manage the bingo cards and associated numbers
	integer :: total_call_nums = 0
	integer :: i, j, k   ! do loop variable
	integer, allocatable :: numbersArray(:) ! imported bingo 'call numbers' array
	integer :: card(5,5), card_nums, win=-1,house,win_card ! bingo card array, total for card numbers and win outcome
	logical :: matches(10,5,5) ! track the game status as numbers are called as truth table
	integer, allocatable :: bingo_cards(:,:,:) ! all bingo numbers called    
	logical, allocatable :: called(:,:,:) ! track numbers drawn to match bingo cards status as truth table
	logical, allocatable :: won(:) ! track numbers that matched bingo cards numbers and won as truth table
	
	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1

	call get_bingo_call_numbers(filename,numbersArray,total_call_nums)
	
	write(*,'(A,I0,A,I0,A)') " » Bingo calls to be made: ",total_call_nums," [array check: ",size(numbersArray),"]"
	!write(*,'(I2.2)') numbersArray ! print all bingo call numbers extracted from input
	
	call setup_bingo_cards(filename, card_nums, card, bingo_cards)
	write(*,'(A,I0)') " » Bingo cards created: ",card_nums
	print *, ""
	
	! allocated arrays to track called numbers and any winning cards sized on total number of cards created 'card_nums'
	allocate(called(card_nums,5,5))
	allocate(won(card_nums))
	
	matches = .false.
	do i=1,5
		matches(i,i,:) = .true.
		matches(i+5,:,i) = .true.
	end do
	
	called = .false.
	won = .false.
	
	outer: do i=1,total_call_nums
		if (win .gt. 0) exit
		!print *, "Called number is: ",numbersArray(i)
		called = called .or. (bingo_cards .eq. numbersArray(i))
		!print *, "Called check is: ", called
		do j=1,card_nums
			!print *, "Checking bingo card: ",j
			if (won(j)) then
				print *, "Skipping winning bingo card: ",j
				cycle ! if card won already - loop through rest still
			end if
			do k=1,10
				!print *, "Card matches are",(count(called(j,:,:) .and. matches(k,:,:)))
				if (count(called(j,:,:) .and. matches(k,:,:)) .eq. 5) then  ! if called number and matches in card row or column
					!print *, "Match is",(count(called(j,:,:) .and. matches(k,:,:)))
					print *, "HOUSE!! Called wining number is: ",numbersArray(i)," for bingo card: ",j
					house = numbersArray(i)*sum(bingo_cards(j,:,:), mask = .not. called(j,:,:))
					if (win .lt. 0) then
						win=house
						win_card = j
					end if
					won(j) = .true.  ! add wining card to 'won' array
					exit
				end if
			end do
		end do
	end do outer
	
	print *, ""
	write(*,'(A,I0)') "First winning bingo card is: ",win_card
	write(*,'(A,I0)') "Final score for bingo card is: ",win
	print *, ""
	
	! clean up arrays allocated
	deallocate(numbersArray)
	deallocate(bingo_cards)
	deallocate(called)
	deallocate(won)
	
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

	! Check bingo cards numbers and setup arrays to hold allocated numbers. 
	! Created management arrays to hold and track called and matched numbers.
	subroutine setup_bingo_cards(filename, card_nums, card, bingo_cards)
		character(len=*), intent(in) :: filename
		integer, intent(inout) :: card_nums, card(5,5)
		integer, allocatable, intent(inout) :: bingo_cards(:,:,:)
		integer :: fileunit, io, i
		
		open(newunit=fileunit, file=filename, action='read', position='rewind')		
		card_nums = 0
		do
			read(fileunit,*,iostat=io)        ! read and skip first line as 'call numbers' - plus subsequent between cards
			if (io /= 0) exit
			read(fileunit,*,iostat=io) card   ! populate bingo card 5x5 array
			if (io /= 0) exit
			card_nums = card_nums + 1         ! track number of bingo cards read
		end do
		
		allocate(bingo_cards(card_nums,5,5))  ! create bingo cards array based on now known total number of cards read
		
		rewind(fileunit)                      ! move back to start of the input file
		
		read(fileunit,*)                          ! read to skip first line as 'call numbers'
		do i=1,card_nums
			read(fileunit,*)                      ! read to skip blank line before card numbers
			read(fileunit,*) bingo_cards(I,:,:)   ! read bingo card number into array
		end do
		
		close(fileunit)
	end subroutine setup_bingo_cards


end program aoc_day04_p1
