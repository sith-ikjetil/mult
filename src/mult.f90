!
! module mult_util
! 
! misc functions and subroutines
!
module mult_util
    implicit none

contains
    !
    ! function string_to_integer
    !
    ! tries to convert a string to a number
    !
    integer function string_to_integer(string_input, version)
        implicit none
        character(*), intent(in)    :: string_input
        character(*), intent(in)    :: version
        integer                     :: integer_output, i, n
        
        n = len(trim(string_input))
        
        integer_output = 0

        do i=1,n
            if (ichar(string_input(i:i)) >= 48 .and. ichar(string_input(i:i)) <= 57) then
                integer_output = integer_output * 10 + ichar(string_input(i:i)) - 48
            else
                write(*,*) ' ABEND: Invalid input string.'
                call PrintUsage(version)
                call exit(1)
            end if
        end do

        string_to_integer = integer_output
    end function string_to_integer

    !
    ! fulnction integer_to_string
    !
    ! converts an integer to string
    !
    character(len=20) function integer_to_string(i)
        integer, intent(in) :: i
        character(len=20)   :: str
        write (str, *) i
        integer_to_string = adjustl(str)
    end function integer_to_string
    
    !
    ! subroutine PrintUsage
    !
    ! prints out usage screen
    !
    subroutine PrintUsage(version)
        character(*), intent(in) :: version
                    
        print *, ' '
        print *, ' Multiplication Table v', version
        print *, ' '
        print *, ' Usage: mult'
        print *, '    or: mult [option]'
        print *, ' '
        print *, '    option         explanation'
        print *, ' -------------  -----------------'
        print *, '    all, 0         show entire multiplication table'
        print *, '   <number>        show multiplication table up to. Max 12.'
        print *, '  -h, --help       shows this help/usage screen'
        print *, ' '
        print *, ' Created by Kjetil Kristoffer Solberg <post@ikjetil.no>'
        print *, ' Written in fortran for the GNU fortran compiler'
        print *, ' '
    end subroutine PrintUsage
end module mult_util

!
! program main
!
! program entrypoint
!
program main
    !
    ! use
    !
    use mult_util

    !
    ! var definition
    !
    implicit none
    character(len=*), parameter :: version = '1.0'
    character(len=32)           :: arg        ! arg value
    integer                     :: arg_count  ! argument count, 0 or 1 valid
    integer                     :: mult_value ! 0 = all, 1-15 = multiplication table, 15-n error
    
    !
    ! program logic
    !
    arg_count = command_argument_count()
    if (arg_count /= 0 .and. arg_count /= 1) then
        call PrintUsage(version)
    else if (arg_count == 0) then
        call PrintTable(1,12, version)
    else if (arg_count == 1) then
        call get_command_argument(1, arg)
        if (arg == 'all') then
            call PrintTable(1, 12, version)
        else if (arg == '-h' .or. arg == '--help') then
            call PrintUsage(version)
        else
            mult_value = string_to_integer(arg, version)
            if (mult_value == 0) then
                call PrintTable(1,12, version)
            else if (mult_value >= 1 .and. mult_value <= 12) then
                call PrintTable(1, mult_value, version)
            else
                call PrintUsage(version)
            endif
        end if
    end if
    call exit(0)

contains
    !
    ! subroutine PrintTable
    !
    ! prints out multiplication table
    !
    subroutine PrintTable(from, to, version)
        implicit none
        character(len=*), intent(in)    :: version
        integer, intent(in)             :: from
        integer, intent(in)             :: to
        integer                         :: i, n
        character(len=50)               :: sep_line
        
        print *, ' '
        print *, '  Multiplication Table v', version 
        print *, ' '

        n = to

        i = 1

        select case (n)
            case (1)
                print 101, i*1
            case (2)
                print 102, i*1, i*2
            case (3)
                print 103, i*1, i*2, i*3
            case (4)
                print 104, i*1, i*2, i*3, i*4
            case (5)
                print 105, i*1, i*2, i*3, i*4, i*5
            case (6)
                print 106, i*1, i*2, i*3, i*4, i*5, i*6
            case (7)
                print 107, i*1, i*2, i*3, i*4, i*5, i*6, i*7
            case (8)
                print 108, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8
            case (9)
                print 109, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9
            case (10)
                print 110, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10
            case (11)
                print 111, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10, i*11
            case (12)
                print 112, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10, i*11, i*12
            case default
                print *, ' ABEND: Invalid runtime error.'
                call PrintUsage(version)
                call exit(1)
        end select

        sep_line = '==='
        do i = 1, n - 1
            sep_line = trim(sep_line)//'===='
        end do

        print *, ' ', trim(sep_line)

        do i = from, to
            select case (n)
                case (1)
                    print 101, i*1
                case (2)
                    print 102, i*1, i*2
                case (3)
                    print 103, i*1, i*2, i*3
                case (4)
                    print 104, i*1, i*2, i*3, i*4
                case (5)
                    print 105, i*1, i*2, i*3, i*4, i*5
                case (6)
                    print 106, i*1, i*2, i*3, i*4, i*5, i*6
                case (7)
                    print 107, i*1, i*2, i*3, i*4, i*5, i*6, i*7
                case (8)
                    print 108, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8
                case (9)
                    print 109, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9
                case (10)
                    print 110, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10
                case (11)
                    print 111, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10, i*11
                case (12)
                    print 112, i*1, i*2, i*3, i*4, i*5, i*6, i*7, i*8, i*9, i*10, i*11, i*12
                case default
                    print *, ' ABEND: Invalid runtime error.'
                    call PrintUsage(version)
                    call exit(1)
            end select
        end do
        
        print *, ' '
    !
    ! format
    !
    101 format ( ' ', i3)
    102 format ( ' ', i3, ' ', i3)
    103 format ( ' ', i3, ' ', i3, ' ', i3)
    104 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    105 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    106 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    107 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    108 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    109 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    110 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    111 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    112 format ( ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3, ' ', i3)
    end subroutine PrintTable

end program main

