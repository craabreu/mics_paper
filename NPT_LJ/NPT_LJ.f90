program NTP_LJ

use mTypes
use mUtil

implicit none

! ====== Parameters ======
real(rb), parameter :: One_Third = 1.0_rb/3.0_rb
integer,  parameter :: Out = 15

! ====== Input data ======
character(sl) :: Base
integer(ib)   :: N, NEC, NPC, Seed, iProdData, iShow
real(rb)      :: T, P, L, rc, Np1

! ===== System Variables ======
real(rb) :: Rho, E, W, V, lnV, Acpt_Ratio
real(rb), allocatable :: R(:,:)

! ===== Radial Distribution Function Variables =====
integer(ib) :: NBins
real(rb)    :: rmin, rmax, r2min, r2max
integer(2), allocatable :: Hbin(:), Delta_Hbin(:)
real(rb), allocatable    :: r2bin(:)

! ====== Auxiliary variables ======
integer(ib)   :: i, j, k, icycle, NAcpt, NAcptV, Line = 0_ib, Nplus, NAtt, NAttV
real(rb)      :: dr_max, dr_V, dlnV_max, Beta, Minus_Beta, Inv_N, rc2, L_2, mL_2
real(rb)      :: ELRCC, WLRCC

integer(ib) :: NProps

! ====== Start ======
write(*,'("+-----------------------------------------------+")')
write(*,'("|   Isothermal-Isobaric Ensemble Monte Carlo    |")')
write(*,'("|      Simulation of Lennard-Jones Fluids       |")')
write(*,'("+-----------------------------------------------+")')

call Read_Data
call Define_Variables
call Lattice_Configuration

dr_V     = 8.e-4_rb
dr_max   = dr_V*V
dlnV_max = 0.1_rb

call Long_Range_Correction_Constants( rc, ELRCC, WLRCC )

write(*,'(/,"Starting equilibration",/)')
call Total_Energy_and_Virial( E, W )

!call Initialize_Storage( trim(Base)//'_eq.out', Prop_Names(NBins) )
NAcpt  = 0 ; NAcptV = 0 ; NAtt = 0 ; NAttV = 0
Nplus = nint(N/0.99_rb,ib)

do icycle = 1, NEC
  do i = 1, N
    j = Random_Integer( Nplus )
    if (j <= N) then
      NAtt = NAtt + 1_ib
      call Monte_Carlo_Step( j )
    else
      NAttV = NAttV + 1
      call Volume_Move
    end if
  end do
!  if (mod(icycle,iEquilData) == 0) call Store_Data()
!  if (mod(icycle,iUpdate) == 0)    call Update_dr_max
!  if (mod(icycle,iUpdate*N) == 0)  call Update_dlnV_max
  if (mod(icycle,iShow) == 0)      call Show_Data( icycle )
end do
call Close_Table

write(*,'(/,"Starting production",/)')
call Initialize_Storage( trim(Base)//'.dat', Prop_Names(NBins) )
call Total_Energy_and_Virial( E, W )
NAcpt  = 0 ; NAcptV = 0 ; NAtt = 0 ; NAttV = 0

k = 0
do icycle = NEC+1, NEC+NPC
  do i = 1, N
    j = Random_Integer( Nplus )
    if (j <= N) then
      NAtt = NAtt + 1_ib
      call Monte_Carlo_Step( j )
    else
      NAttV = NAttV + 1
      call Volume_Move
    end if
  end do
  if (mod(icycle,iProdData) == 0) call Store_Data()
  if (mod(icycle,iShow) == 0) call Show_Data( icycle )
end do
call Close_Table
close(Out)

Acpt_Ratio = dble(NAcpt)/dble(NAtt)
write(*,'(/,"Acceptance ratio during production run:  ",F6.4)') Acpt_Ratio

Acpt_Ratio = dble(NAcptV)/dble(NAttV)
write(*,'(/,"Volume acc. ratio during production run: ",F6.4)') Acpt_Ratio

write(*,'(/,"Energy and virial from the simulation: ",2E20.11)') E, W
call Total_Energy_and_Virial( E, W )
write(*,'(  "Energy and virial of the final state:  ",2E20.11)') E, W

contains

!===============================================================================

  subroutine Close_Table

    call Finish_Table( unit = 6 )
    Line = 0_ib

  end subroutine Close_Table

!===============================================================================

  subroutine Define_Variables

    integer(ib) :: i
    real(rb)    :: log_rmin, log_rmax, delta, log_r

    Beta = 1.0_rb/T                     ! Inverse temperature
    Minus_Beta = -Beta                  ! Reciprocal-inverse temperature
    Inv_N = 1.0_rb/dble(N)              ! Inverse number of molecules
    allocate( R(3,N) )                  ! Reduced positions
    rc2 = rc**2                         ! Square of the reduced cutoff

    dr_max = 0.02_rb
    dlnV_max = 0.02_rb

    r2min = rmin**2
    r2max = rmax**2
    allocate( r2bin(NBins), Hbin(NBins), Delta_Hbin(NBins) )

    log_rmin = log10(rmin)
    log_rmax = log10(rmax)
    delta = (log_rmax - log_rmin)/NBins
    do i = 1, NBins
      log_r = log_rmin + i*delta
      r2bin(i) = 10.0_rb**(2.0_rb*log_r)
    end do

    NProps = 3 + NBins

    Np1 = real(N + 1,rb)

  end subroutine Define_Variables

!===============================================================================

  subroutine Energy_and_Virial( i, Ri, Energy, Virial, Const )

    integer(ib), intent(in)  :: i, Const
    real(rb),    intent(in)  :: Ri(3)
    real(rb),    intent(out) :: Energy, Virial

    integer(ib) :: j, bin
    real(rb)    :: Eij_4, W_8, Rij(3), r2

    Energy = 0.0_rb
    Virial = 0.0_rb
    do j = 1, N
      if (j /= i) then
        Rij = PBC( R(:,j) - Ri )
        r2 = sum(Rij**2)
        if (r2 < rc2) then
          call Lennard_Jones( r2, Eij_4, W_8 )
          Energy = Energy + Eij_4
          Virial = Virial + W_8
        end if
        if ((r2 >= r2min).and.(r2 < r2max)) then
          bin = 1
          do while (r2 > r2bin(bin))
            bin = bin + 1
          end do
          Delta_Hbin(bin) = Delta_Hbin(bin) + Const
        end if
      end if
    end do
    Energy = 4.0_rb*Energy
    Virial = 8.0_rb*Virial

  end subroutine Energy_and_Virial

!===============================================================================

  subroutine Lattice_Configuration

    integer(ib) :: M, i, j, k, q
    real(rb)    :: Inv_M, HL(3), L0, L_id

    M = int( dble(N)**(1.d0/3.d0) )
    if (M**3 < N) M = M + 1
    Inv_M = 1.0_rb/dble(M)
    do q = 1, N
      i = q/M**2
      j = (q - i*M**2)/M
      k = q - (j + i*M)*M
      R(:,q) = dble( (/i,j,k/) + 1)
    end do
    do i = 1, 3
      R(i,:) = R(i,:) - 0.5_rb*(maxval(R(i,:))+minval(R(i,:)))
      HL(i) = maxval(R(i,:))
    end do

    L0 = 2.0_rb*maxval(HL) + 1.0_rb

    L_id = (N*T/P)**One_Third
    if (T <= 1.3) then
      if ( log(P) > 3.07 - 6.75/T ) then
        L_id = 0.01d0*L_id
      end if
    end if

    if (L_id > L0) then
      L = L_id
      R = R*L_id/L0
    else
      L = L0
    end if
    V = L**3
    Rho = N/V
    lnV = log(V)
    L_2  = 0.5_rb*L
    mL_2 = -L_2

  end subroutine Lattice_Configuration

!===============================================================================

  subroutine Lennard_Jones( r2, E_4, W_8 )

    real(rb), intent(in)  :: r2
    real(rb), intent(out) :: E_4, W_8

    real(rb) :: x, x2

    x  = 1.0_rb/(r2*r2*r2)
    x2 = x*x
    E_4 = x2 - x
    W_8 = x2 + E_4

  end subroutine Lennard_Jones

!===============================================================================

  subroutine Long_Range_Correction_Constants( rc, ELRCC, WLRCC )

    real(rb), intent(in)  :: rc
    real(rb), intent(out) :: ELRCC, WLRCC

    real(rb), parameter :: Pi_8_3 = 8.3775804095727819692337156887453_rb
    real(rb) :: SR3, SR9_3, A

    A     = Pi_8_3*N
    SR3   = (1.0_rb/rc)**3
    SR9_3 = One_Third*SR3**3
    ELRCC = A*(SR9_3 - SR3)
    WLRCC = A*(4.0_rb*SR9_3 - 2.0_rb*SR3)

  end subroutine Long_Range_Correction_Constants

!===============================================================================

  subroutine Monte_Carlo_Step( i )

    integer(ib), intent(in) :: i

    real(rb) :: Ri_o(3), Ri_n(3), E_o, E_n, Delta_E, W_o, W_n
    logical  :: Accept

    Ri_o = R(:,i)

    Ri_n = PBC( Ri_o + ((/Random(),Random(),Random()/) - 0.5_rb)*dr_max )

    Delta_Hbin = 0
    call Energy_and_Virial( i, Ri_n, E_n, W_n, +1 )
    call Energy_and_Virial( i, Ri_o, E_o, W_o, -1 )

    Delta_E = E_n - E_o

    Accept = Delta_E <= 0.0_rb
    if (.not.Accept) Accept = Random() < exp(Minus_Beta*Delta_E)

    if (Accept) then
      NAcpt = NAcpt + 1
      R(:,i) = Ri_n
      E = E + Delta_E
      W = W + W_n - W_o
      Hbin = Hbin + Delta_Hbin
    end if

  end subroutine Monte_Carlo_Step

!===============================================================================

  elemental function PBC( X ) result( Xn )

    real(rb), intent(in) :: X
    real(rb)             :: Xn

    if (X > L_2) then
      Xn = X - L
    else if (X < mL_2) then
      Xn = X + L
    else
      Xn = X
    end if

  end function PBC

!===============================================================================

  subroutine Read_Data

    integer(ib) :: NShow
    real(rb) :: P_T

    read(*,*); read(*,*) Base       ! Base for file names
    read(*,*); read(*,*) N          ! Number of atoms
    read(*,*); read(*,*) T          ! Reduced temperature
    read(*,*); read(*,*) P_T        ! Reduced pressure/temperature
    read(*,*); read(*,*) rc         ! Reduced cutoff distance
    read(*,*); read(*,*) NEC        ! Number of equilibration cycles
    read(*,*); read(*,*) NPC        ! Number of production cycles
    read(*,*); read(*,*) Seed       ! Seed for random numbers
    read(*,*); read(*,*) iProdData  ! Interval for property storage during production
    read(*,*); read(*,*) NShow      ! Times to show results on screen
    read(*,*); read(*,*) rmin, rmax
    read(*,*); read(*,*) NBins

    iShow = (NEC + NPC) / NShow
    P = T*P_T

    call Randomize( Seed )

    write(*,'(/,"Number of particles..............: ",I12  )') N
    write(*,'(  "Reduced temperature..............: ",F12.7)') T
    write(*,'(  "Reduced pressure.................: ",F12.7)') P
    write(*,'(  "Reduced cutoff distance .........: ",F12.7)') rc
    write(*,'(  "Number of equilibration cycles...: ",I12  )') NEC
    write(*,'(  "Number of production cycles......: ",I12,/)') NPC

  end subroutine Read_Data

!===============================================================================

  subroutine Show_Data( icycle )

    integer(ib), intent(in) :: icycle

    character(7) :: Percentage

    if (Line == 0) then
      call Start_Table( 6, (/character(9)::"Cycle","Energy/N","Pressure", "Concluded"/), &
                        (/10,18,18,9/) )
    else if (mod(Line,21_ib) == 0_ib) then
      call Print_Table_Titles( unit = 6 )
    end if

    write(Percentage,'(F5.1," %")') dble(icycle)/dble(NEC+NPC)*100.d0
    call Insert_Table_Cell( 6, icycle )
    call Insert_Table_Cell( 6, E*Inv_N )
    call Insert_Table_Cell( 6, Rho*T + W/V )
    call Insert_Table_Cell( 6, Percentage )

    Line = Line + 1_ib

  end subroutine Show_Data

!===============================================================================

  subroutine Total_Energy_and_Virial( Energy, Virial )

    real(rb), intent(out) :: Energy, Virial

    integer(ib) :: i, j, bin
    real(rb)    :: Eij_4, W_8, Ri(3), Rij(3), r2

    Energy = 0.0_rb
    Virial = 0.0_rb
    Hbin = 0
    do i = 1, N-1
      Ri = R(:,i)
      do j = i + 1, N
        Rij = PBC( R(:,j) - Ri )
        r2 = sum( Rij**2 )
        if (r2 <= rc2) then
          call Lennard_Jones( r2, Eij_4, W_8 )
          Energy = Energy + Eij_4
          Virial = Virial + W_8
        end if
        if ((r2 >= r2min).and.(r2 < r2max)) then
          bin = 1
          do while (r2 > r2bin(bin))
            bin = bin + 1
          end do
          Hbin(bin) = Hbin(bin) + 1
        end if
      end do
    end do
    Energy = 4.0_rb*Energy + ELRCC*Rho
    Virial = 8.0_rb*Virial + WLRCC*Rho

  end subroutine Total_Energy_and_Virial

!===============================================================================
!
!  subroutine Update_dlnV_max
!
!    real(rb), parameter :: Target = 0.5_rb, Variation = 0.05_rb
!
!    if (dble(NAcptV)/dble(iUpdate) > Target) then
!      dlnV_max = (1.0_rb + Variation)*dlnV_max
!    else
!      dlnV_max = (1.0_rb - Variation)*dlnV_max
!    end if
!    NAcptV = 0
!
!  end subroutine Update_dlnV_max
!
!===============================================================================
!
!  subroutine Update_dr_max
!
!    real(rb), parameter :: Target = 0.5_rb, Variation = 0.05_rb
!
!    if (dble(NAcpt)/dble(N*iUpdate) > Target) then
!      dr_V = (1.0_rb + Variation)*dr_V
!    else
!      dr_V = (1.0_rb - Variation)*dr_V
!    end if
!    NAcpt = 0
!
!    dr_V = min( dr_V, 0.499_rb )
!    dr_max = dr_V*V
!
!  end subroutine Update_dr_max
!
!===============================================================================

  subroutine Volume_Move

    real(rb) :: Old_V, Old_L, Old_Hbin(NBins), Old_Rho, Old_R(3,N)
    real(rb) :: New_lnV, New_lnL, New_E, New_W
    real(rb) :: Exponent
    logical  :: Accept

    Old_V = V
    Old_L = L
    Old_Hbin = Hbin
    Old_Rho = Rho
    Old_R = R

    New_lnV = lnV + (Random() - 0.5_rb)*dlnV_max
    New_lnL = One_Third*New_lnV
    L = exp(New_lnL)
    L_2 = 0.5_rb*L
    mL_2 = -L_2
    V = L**3
    Rho = N/V

    R = R*L/Old_L
    call Total_Energy_and_Virial( New_E, New_W )

    Exponent= Minus_Beta*(New_E - E + P*(V - Old_V)) + Np1*(New_lnV - lnV)

    Accept = Exponent >= 0.0_rb
    if (.not.Accept) Accept = Random() < exp(Exponent)

    if (Accept) then
      NAcptV = NAcptV + 1
      E = New_E
      W = New_W
      lnV = New_lnV
      dr_max = dr_V * V
    else
      V = Old_V
      L = Old_L
      L_2 = 0.5_rb*L
      mL_2 = -L_2
      Hbin = Old_Hbin
      Rho = Old_Rho
      R = Old_R
    end if

  end subroutine Volume_Move

!===============================================================================

  subroutine Initialize_Storage( File, Properties )

    character(*), intent(in) :: File, Properties(:)

    integer(ib) :: i, NProps

    NProps = size(Properties)
    open(unit=Out, file=File, status='replace' )
    do i = 1, NProps-1
      write(Out,'(A," ")',advance='no') trim(Properties(i))
    end do
    write(Out,'(A)') trim(Properties(NProps))

  end subroutine Initialize_Storage

!===============================================================================

  subroutine Store_Data( )

    write(Out,*) E, N/V, W/V, (Hbin(k),k=1,NBins)

  end subroutine Store_Data

!===============================================================================

  function Prop_Names(NBins) result( Names )
    integer, intent(in) :: NBins

    character(6) :: Names(NBins+3)

    integer(ib)  :: i
    character(3) :: C

    Names(1:3) = (/"Energy","Volume","Virial"/)
    do i = 1, NBins
      write(C,'(I3)') i
      Names(i+3) = "N"//trim(adjustl(C))
    end do

  end function Prop_Names

!===============================================================================

end program NTP_LJ
