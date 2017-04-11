module mUtil

use mTypes

implicit none 

!===============================================================================
!                              C O N S T A N T S                               !
!===============================================================================

real(rb), parameter, public :: Pi = 3.1415926535897932384626433832795_rb

!===============================================================================
!                                  F I L E S                                   !
!===============================================================================

character, private :: Default_Delimiter = ","
logical,   private :: New_Line = .true.

interface dwrite
  module procedure Write_Character
  module procedure Write_Integer_Scalar
  module procedure Write_Real_Scalar
  module procedure Write_Integer_Vector
  module procedure Write_Real_Vector
end interface dwrite

public  :: dwrite

private :: Write_Character
private :: Write_Integer_Scalar
private :: Write_Real_Scalar
private :: Write_Integer_Vector
private :: Write_Real_Vector

!===============================================================================
!                             M A T H E M A T I C S                            !
!===============================================================================

! Definition of dot (inner) product:
interface operator (.dot.)
  module procedure DotProduct
end interface

! Definition of cross (vector) product:
interface operator (.x.)
  module procedure CrossProduct
end interface

private :: DotProduct
private :: CrossProduct

interface Log_Sum_Exp
  module procedure Log_Sum_Exp_1
  module procedure Log_Sum_Exp_2
  module procedure Log_Sum_Exp_3
end interface

!===============================================================================
!                        R A N D O M      N U M B E R S                        !
!===============================================================================

integer(4), private :: idum, iix, iiy, iik
real(8),    private :: aam

!===============================================================================
!                                 T A B L E S                                  !
!===============================================================================

integer(ib),   private :: Column
integer(ib),   private :: NColumns
character(sl), private :: Table_Titles
character(sl), private :: Table_Edges

character(2),  allocatable, private :: Col_Len(:)
character(2),  allocatable, private :: NDigts(:)

interface Insert_Table_Cell
  module procedure Insert_Table_Cell_Char
  module procedure Insert_Table_Cell_Integer
  module procedure Insert_Table_Cell_Real
end interface

!===============================================================================
!                                   T I M E                                    !
!===============================================================================

type RealTime
  integer(4) :: Hours
  integer(1) :: Minutes,  &
                Seconds
  integer(2) :: MiliSecs
end type RealTime

interface operator (+)
  module procedure Add_Times
end interface

interface operator (-)
  module procedure Subtract_Times
end interface

interface operator (*)
  module procedure Multiply_Number_By_Time
end interface

interface assignment(=)
  module procedure Assign_Time
end interface

type(RealTime), private :: Initial_Time

!===============================================================================

contains

!###############################################################################
!#                          A L G O R I T H M S                                #
!###############################################################################

  subroutine Sort( X, Ind, N )

    integer(ib), intent(in)  :: N
    integer(ib), intent(out) :: Ind(N)
    real(rb), intent(inout)  :: X(N)

    integer(ib) :: i

    ! A recursive sorting routine Adapted from The design of Well-Structured
    ! and Correct Programs, S. Alagic, Springer-Verlag, 1978.

    forall (i=1:N) Ind(i) = i
    call Qsort( X, Ind, 1, N, N )

    contains

      recursive subroutine Qsort( X, Ind, M, N, NN )

        integer(ib), intent(in) :: M, N, NN
        integer(ib), intent(inout) :: Ind(NN)
        real(rb), intent(inout) :: X(NN)

        integer :: I, J

        if (M < N) then
          call Partit(X, I, J, M, N, Ind, NN) ! divide in two
          call Qsort(X, Ind, M, J, NN)        ! Sort left part
          call Qsort(X, Ind, I, N, NN)        ! Sort right part
        end if

      end subroutine Qsort

      subroutine Partit( A, I, J, Left, Right, Ind, NN )

        integer(ib), intent(in) :: Left, Right, NN
        integer(ib), intent(inout) :: I, J
        real(rb), intent(inout) :: A(NN)
        integer(ib), intent(inout) :: Ind(NN)

        integer(ib) :: IAux
        real(rb) :: Pivot, Aux

        Pivot = A( (Left + Right)/2 )
        I = Left
        J = right
        do while (I <= J)
          do while (A(I) < Pivot)
            I = I + 1
          end do
          do while (Pivot < A(J))
            J = J - 1
          end do
          if (I <= J) then
            Aux  = A(I)
            A(I) = A(J)
            A(J) = Aux
            IAux = Ind(I)
            Ind(I) = Ind(J)
            Ind(J) = IAux
            I = I + 1
            J = J - 1
          end if
        end do

      end subroutine Partit

  end subroutine Sort

!###############################################################################
!#                  C H A R A C T E R       S T R I N G S                      #
!###############################################################################

  function Array_To_Str( a ) result ( s )

    character, intent(in) :: a(:)
    character(size(a)) :: s

    integer :: i

    do i = 1 , size(a)
      s(i:i) = a(i)
    end do

  end function Array_To_Str

!===============================================================================

  subroutine Change_To_Upper_Case( String )

    character(*), intent(inout) :: String

    integer :: i, Code

    do i = 1, len_trim(String)
      Code = ichar(String(i:i))
      if ( (Code >= 97) .and. (Code <= 122) ) String(i:i) = char(Code-32)
    end do

  end subroutine Change_To_Upper_Case

!###############################################################################
!#                               F I L E S                                     #
!###############################################################################

  function Exists( FileName ) result( Status )

    character(*) FileName

    logical :: Status

    inquire( file = FileName, exist = Status )

  end function Exists

!===============================================================================

  subroutine File_Series( File, Index, Digits )

    integer(ib),  intent(in)    :: Index, Digits
    character(*), intent(inout) :: File

    integer(ib) :: i, j, D, Digit(0:Digits-1), Ext, Last
    character   :: Str
    character(10) :: Extension

    do i = Digits-1, 0, -1
      D = Index
      do j = Digits-1, i + 1, -1
        D = D - 10**j*Digit(j)
      end do
      D = D / 10**i
      Digit(i) = D
    end do

    Last = len_trim( File )
    Ext  = scan( File, '.' )

    if ( Ext /= 0 ) then
      Extension  = File(Ext:Last)
      File = File(1:Ext-1)//'_'
    else
      Extension = ''
      File = File(1:Last)//'_'
    end if

    do i = Digits-1, 0, -1
      write(Str,'(i1)') Digit(i)
      File = trim(File)//Str
    end do
    File = trim(File)//trim(Extension)

  end subroutine File_Series

!===============================================================================

  subroutine Break_Line( unit )

    integer, intent(in) :: unit

    write(unit,fmt='()',advance='yes')
    New_Line = .true.

  end subroutine Break_Line

!===============================================================================

  subroutine Write_Character( unit, var, delimiter )

    integer,      intent(in)           :: unit
    character(*), intent(in)           :: var
    character,    intent(in), optional :: delimiter

    character :: dlm

    if (present(delimiter)) then
      dlm = delimiter
    else
      dlm = Default_Delimiter
    end if

    if (New_Line) then
      write(unit,'(A)',advance='no') var
    else
      write(unit,'("'//dlm//'",A)',advance='no') var
    end if
    New_Line = .false.

  end subroutine Write_Character

!===============================================================================

  subroutine Write_Integer_Scalar( unit, var, fmt, delimiter )

    integer,      intent(in)           :: unit
    integer(ib),  intent(in)           :: var
    character(*), intent(in), optional :: fmt
    character,    intent(in), optional :: delimiter

    character :: dlm
    character(10) :: cvar

    if (present(delimiter)) then
      dlm = delimiter
    else
      dlm = Default_Delimiter
    end if

    if (present(fmt)) then
      if (New_Line) then
        write(unit,'('//fmt//')',advance='no') var
      else
        write(unit,'("'//dlm//'",'//fmt//')',advance='no') var
      end if
    else
      write(cvar,'(I10)') var
      if (New_Line) then
        write(unit,'(A)',advance='no') trim(adjustl(cvar))
      else
        write(unit,'("'//dlm//'",A)',advance='no') trim(adjustl(cvar))
      end if
    end if
    New_Line = .false.

  end subroutine Write_Integer_Scalar

!-------------------------------------------------------------------------------

  subroutine Write_Integer_Vector( unit, var, fmt, delimiter )

    integer,      intent(in)           :: unit
    integer(ib),  intent(in)           :: var(:)
    character(*), intent(in), optional :: fmt
    character,    intent(in), optional :: delimiter

    character :: dlm
    integer :: i

    if (present(delimiter)) then
      dlm = delimiter
    else
      dlm = Default_Delimiter
    end if

    if (present(fmt)) then
      do i = 1, size(var)
        call Write_Integer_Scalar( unit, var(i), fmt )
      end do
    else
      do i = 1, size(var)
        call Write_Integer_Scalar( unit, var(i) )
      end do
    end if

  end subroutine Write_Integer_Vector

!===============================================================================

  subroutine Write_Real_Scalar( unit, var, fmt, delimiter )

    integer,      intent(in)           :: unit
    real(rb),     intent(in)           :: var
    character(*), intent(in), optional :: fmt
    character,    intent(in), optional :: delimiter

    character :: dlm
    character(20) :: cvar

    if (present(delimiter)) then
      dlm = delimiter
    else
      dlm = Default_Delimiter
    end if

    if (present(fmt)) then
      if (New_Line) then
        write(unit,'('//fmt//')',advance='no') var
      else
        write(unit,'("'//dlm//'",'//fmt//')',advance='no') var
      end if
    else
      write(cvar,*) var
      if (New_Line) then
        write(unit,'(A)',advance='no') trim(adjustl(cvar))
      else
        write(unit,'("'//dlm//'",A)',advance='no') trim(adjustl(cvar))
      end if
    end if
    New_Line = .false.

  end subroutine Write_Real_Scalar

!-------------------------------------------------------------------------------

  subroutine Write_Real_Vector( unit, var, fmt, delimiter )

    integer,      intent(in)           :: unit
    real(rb),     intent(in)           :: var(:)
    character(*), intent(in), optional :: fmt
    character,    intent(in), optional :: delimiter

    character :: dlm
    integer :: i

    if (present(delimiter)) then
      dlm = delimiter
    else
      dlm = Default_Delimiter
    end if

    if (present(fmt)) then
      do i = 1, size(var)
        call Write_Real_Scalar( unit, var(i), fmt )
      end do
    else
      do i = 1, size(var)
        call Write_Real_Scalar( unit, var(i) )
      end do
    end if

  end subroutine Write_Real_Vector

!###############################################################################
!#                            M A T H E M A T I C S                            #
!###############################################################################

  function DotProduct( a, b ) result( c )

    real(rb), intent(in) :: a(3), b(3)
    real(rb) :: c

    c = dot_product(a,b)

  end function DotProduct

!===============================================================================

  function CrossProduct( a, b ) result( c )

    real(rb), intent(in) :: a(3), b(3)
    real(rb) :: c(3)

    c = (/ a(2)*b(3)-a(3)*b(2), &
           a(3)*b(1)-a(1)*b(3), &
           a(1)*b(2)-a(2)*b(1) /)

  end function CrossProduct

!===============================================================================

  subroutine Gram_Schmidt( U, W )

    real(rb), intent(inout) :: U(3), W(3)

    U = U/sqrt(sum(U*U))
    W = W - sum(U*W)*U
    W = W/sqrt(sum(W*W))

  end subroutine Gram_Schmidt

!===============================================================================

  function Log_Sum_Exp_1( X ) result( Y )

    real(rb), intent(in) :: X(:)
    real(rb)             :: Y

    integer(ib) :: N, imax(1), i
    real(rb)    :: Xs(size(X)), Xmax, Eps, Exponent

    N = size(X)
    imax = maxloc(X)
    Xmax = X(imax(1))
    Xs = X
    Xs(imax(1)) = X(N)
    Eps = log(epsilon(Xmax))
    Y = 1.0_rb
    do i = 1, N-1
      Exponent = Xs(i) - Xmax
      if (Exponent >= Eps) Y = Y + exp(Exponent)
    end do
    Y = Xmax + log(Y)

  end function Log_Sum_Exp_1

!-------------------------------------------------------------------------------

  function Log_Sum_Exp_2( X, Mask ) result( Y )

    real(rb), intent(in) :: X(:)
    logical,  intent(in) :: Mask(:)
    real(rb)             :: Y

    integer(ib) :: N, imax(1), i
    real(rb)    :: Xs(size(X)), Xmax, Eps, Exponent

    N = count(Mask)
    Xs(1:N) = pack(X,Mask)
    imax = maxloc(Xs(1:N))
    Xmax = Xs(imax(1))
    Xs(imax(1)) = Xs(N)
    Eps = log(epsilon(Xmax))
    Y = 1.0_rb
    do i = 1, N-1
      Exponent = Xs(i) - Xmax
      if (Exponent >= Eps) Y = Y + exp(Exponent)
    end do
    Y = Xmax + log(Y)

  end function Log_Sum_Exp_2

!-------------------------------------------------------------------------------

  function Log_Sum_Exp_3( X1, X2 ) result( Y )

    real(rb), intent(in) :: X1, X2
    real(rb)             :: Y

    real(rb) :: Xmin, Xmax

    Xmin = min(X1,X2)
    Xmax = max(X1,X2)
    Y = Xmax + log(1.0_rb + exp(Xmin - Xmax))

  end function Log_Sum_Exp_3

!===============================================================================
!
!  function Ratio_Sum_Exp( A, B, X ) result( Y )
!
!    real(rb), intent(in) :: A(:), B(size(A)), X(size(A))
!
!    ! Y = sum( A(i)*exp(X(i)), i=1..N) / sum( B(i)*exp(X(i)), i=1..N )
!
!    integer(ib) :: N, i, Ind(size(A))
!    real(rb)    :: Xs(size(A)), Ra, Rb, Exp_Term
!
!    N = size(A)
!    Xs = X
!    call Sort( Xs, Ind, N )
!    Ra = A(Ind(1))
!    Rb = B(Ind(1))
!    do i = 2, N
!      Exp_Term = exp( Xs(i-1) - Xs(i) )
!      Ra = A(Ind(i)) + Ra*Exp_Term
!      Rb = B(Ind(i)) + Rb*Exp_Term
!    end do
!    Y = Ra/Rb
!
!  end function Ratio_Sum_Exp
!
!###############################################################################
!#                        R A N D O M      N U M B E R S                       #
!###############################################################################

  subroutine Randomize( Seed )

    integer(ib), intent(in) :: Seed

    ! This subroutine HAS to be called before using any other subroutine or
    ! function in this section.

    idum = -abs(Seed)
    aam  = nearest(1.0,-1.0)/2147483647
    iiy  = ior(ieor(888889999,abs(idum)),1)
    iix  = ieor(777755555,abs(idum))

  end subroutine Randomize

!===============================================================================

  function Random() result( Rand )

    real(rb) :: Rand

    ! "Minimal" random number generator of Park and Miller combined with
    ! a Marsaglia shift sequence. Returns a uniform random deviate between
    ! 0.0 and 1.0 (exclusive of the endpoint values). This fully portable,
    ! generator has the "traditional" (not Fortran 90) calling sequence with
    ! a scalar random deviate as the returned function value: call with idum
    ! a negative integer to initialize; thereafter, do not alter idum except
    ! to reinitialize.
    ! The period of this generator is about 3.1E+18

    integer(4), parameter :: IA = 16807,  IM = 2147483647,  &
                             IQ = 127773, IR = 2836

    iix = ieor(iix,ishft(iix,13))            ! Marsaglia shift sequence with
    iix = ieor(iix,ishft(iix,-17))           ! period 2^32-1.
    iix = ieor(iix,ishft(iix,5))
    iik = iiy/IQ                             ! Park-Miller sequence by 
    iiy = IA*(iiy-iik*IQ)-IR*iik             ! Schrage's method, period 2^31-2.
    if (iiy < 0) iiy = iiy+IM
                                             ! Combine the two generators with
    Rand = aam*ior(iand(IM,ieor(iix,iiy)),1) ! masking to ensure nonzero value.

  end function Random

!===============================================================================

  function Random_Integer( N ) result( irand )

    integer(ib), intent(in) :: N
    integer(ib)             :: irand

    ! This function returns an integer number randomly selected from [1,N].

    irand = int(real(N,rb)*Random(),ib) + 1_ib

  end function Random_Integer

!===============================================================================

  function Random_Unit_Vector() result ( U )

    real(rb) :: U(3)

    ! This function returns a cartesian randomly and uniformly taken from
    ! a sphere of radius 1 centered at the origin.

    real(rb) :: Dot, Sq, X, Y

    Dot = 1.1_rb
    do while ( Dot > 1.0_rb )
      X = 1.0_rb - 2.0_rb*Random()
      Y = 1.0_rb - 2.0_rb*Random()
      Dot = X*X + Y*Y
    end do
    Sq = 2.0_rb*sqrt(1.0_rb - Dot)
    U = (/ X*Sq, Y*Sq, 1.0_rb - 2.0_rb*Dot /)

  end function Random_Unit_Vector

!===============================================================================

  function Random_List( N ) result ( List )

    integer(ib), intent(in) :: N
    integer(ib)             :: List(N)

    ! This function returns an integer vector containing all numbers from the
    ! interval [1,N] in a randomly selected order.

    type TLocalList
      integer(ib) :: I
      real(rb)    :: R
    end type TLocalList

    type (TLocalList) :: LocalList(N), Aux
    integer :: i, j, M
    logical :: Replaced

    do i = 1 , N
      LocalList(i)%R = Random()
    end do

    forall (i=1:N) LocalList(i)%I = i

    call Sort( LocalList%R, LocalList%I, N)

    Replaced = .true.
    M = N - 1
    do while (Replaced)
      Replaced = .false.
      do i = 1 , M
        j = i + 1
        if ( LocalList(i)%R > LocalList(j)%R ) then
          Aux = LocalList(i)
          LocalList(i) = LocalList(j)
          LocalList(j) = Aux
          Replaced = .true.
        end if
      end do
      M = M - 1
    end do

    List = LocalList%I

  end function Random_List

!===============================================================================

  function Random_Sample( M, N ) result( Sample )

    integer(ib), intent(in) :: M, N
    integer(ib)             :: Sample(M)

    ! This function returns an integer vector containing M numbers randomly
    ! sampled from the interval [1,N].

    real(rb)    :: V, quot, Nreal, top
    integer(ib) :: LMI, LM, S, i, j, Samp(M)
    logical     :: Chosen(N)

    LMI = min(M,N-M)
    LM = LMI
    Nreal = N
    top = Nreal - LM
    i = 0_ib
    do while (LM >= 2_ib)
      V = Random()
      S = 0_ib
      quot = top/Nreal
      do while (quot > V)
        S = S + 1_ib
        top = -1.0_rb + top
        Nreal = -1.0_rb + Nreal
        quot = quot*top/Nreal
      end do
      i = i + S + 1_ib
      Samp(LMI-LM+1) = i
      Nreal = -1.0_rb + Nreal
      LM = -1_ib + LM
    end do
    S = int(nint(Nreal)*Random())
    i = i + S + 1_ib
    Samp(LMI) = i

    if (M > N-M) then
      forall (j=1:N) Chosen(j) = any(Samp(1:LMI) == j)
      Sample = pack( (/ (j,j=1,N) /), .not.Chosen )
    else
      Sample = Samp
    end if

  end function Random_Sample

!###############################################################################
!#                               T A B L E S                                   #
!###############################################################################

  subroutine Start_Table( Unit, Titles, Lengths )

    integer,      intent(in) :: Unit
    character(*), intent(in) :: Titles(:)
    integer(ib),  intent(in) :: Lengths(size(Titles))

    integer(ib)   :: i, NC, LS, RS

    NColumns = size(Titles)
    allocate( Col_Len(NColumns), NDigts(NColumns) )

    Table_Titles = "|"
    Table_Edges  = "+"
    do i = 1, NColumns
      write(Col_Len(i),'(I2)') Lengths(i)
      write(NDigts(i), '(I2)') max(0,Lengths(i) - 7)
      NC = min(Lengths(i),len_trim(Titles(i)))
      LS = (Lengths(i) - NC)/2 + 1
      RS = Lengths(i) - NC - LS + 2
      Table_Titles = trim(Table_Titles)// &
                     repeat(" ",LS)//Titles(i)(1:NC)//repeat(" ",RS)//"|"
      Table_Edges = trim(Table_Edges)//repeat("-",LS+NC+RS)//"+"
    end do
    call Print_Table_Titles( Unit )

    Column = 0_ib

  end subroutine Start_Table

!===============================================================================

  subroutine Print_Table_Edges( Unit )

    integer, intent(in) :: Unit

    write(Unit,'(A)') trim(Table_Edges)

  end subroutine Print_Table_Edges

!===============================================================================

  subroutine Print_Table_Titles( Unit )

    integer, intent(in) :: Unit

    write(Unit,'(A)') trim(Table_Edges)
    write(Unit,'(A)') trim(Table_Titles)
    write(Unit,'(A)') trim(Table_Edges)

  end subroutine Print_Table_Titles

!===============================================================================

  subroutine Insert_Table_Cell_Char( Unit, Var )

    integer,      intent(in) :: Unit
    character(*), intent(in) :: Var

    character(3) :: Adv

    Column = Column + 1_ib
    if (Column == NColumns) then
      Adv = 'yes'
    else
      Adv = 'no'
    end if

    if (Column == 1_ib) write(Unit,'("|")',advance='no')

    write(Unit,fmt='(" ",A'//Col_Len(Column)//'," |")',advance=Adv) Var

    if (Column == NColumns) Column = 0_ib

  end subroutine Insert_Table_Cell_Char

!-------------------------------------------------------------------------------

  subroutine Insert_Table_Cell_Integer( Unit, Var )

    integer, intent(in)     :: Unit
    integer(ib), intent(in) :: Var

    character(3) :: Adv

    Column = Column + 1_ib
    if (Column == NColumns) then
      Adv = 'yes'
    else
      Adv = 'no'
    end if

    if (Column == 1_ib) write(Unit,'("|")',advance='no')

    write(Unit,fmt='(" ",I'//Col_Len(Column)//'," |")',advance=Adv) Var

    if (Column == NColumns) Column = 0_ib

  end subroutine Insert_Table_Cell_Integer

!-------------------------------------------------------------------------------

  subroutine Insert_Table_Cell_Real( Unit, Var )

    integer,  intent(in) :: Unit
    real(rb), intent(in) :: Var

    character(3) :: Adv

    Column = Column + 1_ib
    if (Column == NColumns) then
      Adv = 'yes'
    else
      Adv = 'no'
    end if

    if (Column == 1_ib) write(Unit,'("|")',advance='no')

    write(Unit,fmt='(" ",ES'//Col_Len(Column)//'.'//NDigts(Column)// &
                   '," |")',advance=Adv) Var

    if (Column == NColumns) Column = 0_ib

  end subroutine Insert_Table_Cell_Real

!===============================================================================

  subroutine Finish_Table( Unit )

    integer, intent(in) :: Unit

    call Print_Table_Edges( Unit )

    NColumns = 0_ib
    Column   = 0_ib
    Table_Titles = ""
    Table_Edges = ""
    if (allocated(Col_Len)) deallocate( Col_Len , NDigts )

  end subroutine Finish_Table

!###############################################################################
!#                                 T I M E                                     #
!###############################################################################

  subroutine Start_Time_Counter

    Initial_Time = Get_Relative_Time()

  end subroutine Start_Time_Counter

!===============================================================================

  function Elapsed_Time() result ( Time )

    type(RealTime) :: Time

     Time = Get_Relative_Time() - Initial_Time

  end function Elapsed_Time

!===============================================================================

  subroutine Assign_Time( Time1, Time2 )

    type(RealTime), intent(out) :: Time1
    type(RealTime), intent(in)  :: Time2

    Time1%Hours    = Time2%Hours
    Time1%Minutes  = Time2%Minutes
    Time1%Seconds  = Time2%Seconds
    Time1%MiliSecs = Time2%MiliSecs

  end subroutine Assign_Time

!===============================================================================

  function Add_Times( Time1, Time2 ) result( ResultTime )

    type(RealTime), intent(in) :: Time1, Time2
    type(RealTime) :: ResultTime

    integer(4) :: H, M, S, MS

    H = Time1%Hours + Time2%Hours
    M = Time1%Minutes + Time2%Minutes
    S = Time1%Seconds + Time2%Seconds
    MS = Time1%MiliSecs + Time2%MiliSecs

    if (MS >= 1000_4) then
      MS = MS - 1000_4
      S = S + 1_4
    end if

    if (S >= 60_4) then
      S = S - 60_4
      M = M + 1_4
    end if

    if (M >= 60_4) then
      M = M - 60_4
      H = H + 1_4
    end if

    ResultTime = RealTime( H, M, S, MS )

  end function Add_Times

!===============================================================================

  function Subtract_Times( Time1, Time2 ) result( ResultTime )

    type(RealTime), intent(in) :: Time1, Time2
    type(RealTime) :: ResultTime

    integer(4) :: H, M, S, MS

    H  = Time1%Hours - Time2%Hours
    M  = Time1%Minutes  - Time2%Minutes
    S  = Time1%Seconds  - Time2%Seconds
    MS = Time1%MiliSecs - Time2%MiliSecs

    if (MS < 0_4) then
      MS = 1000_4 + MS
      S = S - 1_4
    end if

    if (S < 0_4) then
      S = 60_4 + S
      M = M - 1_4
    end if

    if (M < 0_4) then
      M = 60_4 + M
      H = H - 1_4
    end if

    ResultTime = RealTime( H, M, S, MS )

  end function Subtract_Times

!===============================================================================

  function Multiply_Number_By_Time( Number, Time ) result( ResultTime )

    real(8),        intent(in) :: Number
    type(RealTime), intent(in) :: Time
    type(RealTime) :: ResultTime

    integer(4) :: H, M, S, MS

    H  = Time%Hours
    M  = Time%Minutes
    S  = Time%Seconds
    MS = Time%MiliSecs

    MS = nint( Number*(MS + 1000_4*(S + 60_4*(M + 60_4*H))) )
    H  = 0_4 ; M  = 0_4 ; S  = 0_4

    S  = MS/1000_4
    MS = MS - 1000_4*S

    M  = S/60_4
    S  = S - 60_4*M

    H  = M/60_4
    M  = M - 60_4*H

    ResultTime = RealTime( H, M, S, MS )

  end function Multiply_Number_By_Time

!===============================================================================

  function Get_Relative_Time() result ( Time )

    type(RealTime) :: Time

    integer, parameter :: DM(12) = (/  0,  31,  59,  90, 120, 151, &
                                     181, 212, 243, 273, 304, 334 /)

    integer :: DateTime(8)
    integer(4) :: Year, Month, Day, Hour, NY, NCD, NCH
    integer(1) :: Minute, Second
    integer(2) :: MiliSec

    call date_and_time( values = DateTime )

    Year    = DateTime(1)
    Month   = DateTime(2)
    Day     = DateTime(3)
    Hour    = DateTime(5)
    Minute  = DateTime(6)
    Second  = DateTime(7)
    MiliSec = DateTime(8)

    NY = Year - 1901_4                               ! Number of concluded years
    NCD = 365_4*NY + NY/4_4 + DM(Month) + Day - 1_4  ! Number of concluded days
    if (mod(Year,4_4) == 0_4) NCD = NCD + 1_4        ! Bissextile correction
    NCH = 24_4*NCD + Hour                            ! Number of concluded hours

    Time = RealTime( NCH, Minute, Second, MiliSec )

  end function Get_Relative_Time

!===============================================================================

  function Time_To_Str( Time ) result ( StrTime )

    type(RealTime), intent(in) :: Time
    character(20) :: StrTime

    character(2) :: CM, CS
    character(3) :: CMS, CNCH
    integer      :: NCH

    write(CM, '(I2)') Time%Minutes
    write(CS, '(I2)') Time%Seconds
    write(CMS,'(I3)') Time%MiliSecs

    select case (Time%Hours)
      case (:-1)
        NCH = int(log10(real(-Time%Hours))) + 2
      case (0)
        NCH = 1
      case (1:)
        NCH = int(log10(real(Time%Hours))) + 1
    end select
    write(CNCH,'(I3)') NCH

    if (CM(1:1)  == ' ') CM(1:1)  = '0'
    if (CS(1:1)  == ' ') CS(1:1)  = '0'
    if (CMS(1:1) == ' ') CMS(1:1) = '0'
    if (CMS(2:2) == ' ') CMS(2:2) = '0'

    write(StrTime,fmt='(I'//CNCH//',":",A2,":",A2,".",A3)') &
              Time%Hours, CM, CS, CMS

    StrTime = adjustl(StrTime)

  end function Time_To_Str

!===============================================================================

  function Str_Date_And_Time( Time ) result ( String )

    type(RealTime), intent(in), optional :: Time

    character(28) :: String

    character(3), parameter :: StrMonth(12) = (/'Jan', 'Feb', 'Mar', 'Apr', &
                                                'May', 'Jun', 'Jul', 'Aug', &
                                                'Sep', 'Oct', 'Nov', 'Dec' /)

    character(2) :: CD, CH, CM, CS, Ord = 'th'
    character(4) :: CY
    integer      :: DateTime(8)
    integer(4)   :: Year, Month, Day, Hour
    integer(1)   :: Minute, Second

    if (present(Time)) then
      DateTime = Time_To_DateTime( Time )
    else
      call date_and_time( values = DateTime )
    end if

    Year    = DateTime(1)
    Month   = DateTime(2)
    Day     = DateTime(3)
    Hour    = DateTime(5)
    Minute  = DateTime(6)
    Second  = DateTime(7)

    select case (mod(Day,10))
      case (1)
        if (Day /= 11) Ord = 'st'
      case (2)
        if (Day /= 12) Ord = 'nd'
      case (3)
        if (Day /= 13) Ord = 'rd'
    end select

    write(CD,'(I2)') Day
    write(CY,'(I4)') Year
    write(CH,'(I2)') Hour
    write(CM,'(I2)') Minute
    write(CS,'(I2)') Second

    if (CM(1:1) == " ") CM(1:1) = "0"
    if (CS(1:1) == " ") CS(1:1) = "0"

    String = StrMonth(Month)//" "//CD//Ord//", "//CY// &
             " at "//CH//":"//CM//":"//CS//" h"

  end function Str_Date_And_Time

!===============================================================================

  function Time_To_DateTime( Time ) result (DateTime )

    type(RealTime), intent(in) :: Time
    integer                    :: DateTime(8)

    integer(4) :: NCD, NYC, NRD, NRY, NCY, NCM

    integer, parameter :: DM(12)  = (/ 31,  59,  90, 120, 151, 181, &
                                      212, 243, 273, 304, 334, 365 /)
    integer, parameter :: DMB(12) = (/ 31,  60,  91, 121, 152, 182, &
                                      213, 244, 274, 305, 335, 366 /)

    NCD = Time%Hours/24_4
    NYC = NCD / 1461_4
    NRD = NCD - 1461_4*NYC
    if (NRD < 1095_4) then
      NRY = NRD/365_4
      NCY = 4_4*NYC + NRY
      NCD = NRD - 365_4*NRY
      NCM = 0
      do while ( NCD > DM(NCM+1) )
        NCM = NCM + 1
      end do
      NCD = NCD - DM(NCM)
    else
      NCY = 4_4*NYC + 3_4
      NCD = NRD - 1095_4
      NCM = 0
      do while ( NCD > DMB(NCM+1) )
        NCM = NCM + 1
      end do
      NCD = NCD - DMB(NCM)
    end if

    DateTime(1) = NCY + 1901
    DateTime(2) = NCM + 1
    DateTime(3) = NCD + 1
    DateTime(5) = mod(Time%Hours,24_4)
    DateTime(6) = Time%Minutes
    DateTime(7) = Time%Seconds
    DateTime(8) = Time%MiliSecs

  end function Time_To_DateTime

!===============================================================================

  function Estimated_End_Time( Percent_Concluded ) result ( String )

    real(8), intent(in) :: Percent_Concluded
    character(28)       :: String

    String = Str_Date_And_Time( Initial_Time + &
                            (100.d0/Percent_Concluded)*Elapsed_Time() )

  end function Estimated_End_Time

!###############################################################################

end module mUtil
