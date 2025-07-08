program difusao_calor
  implicit none

  integer, parameter :: nx = 50, ny = 50
  integer :: i, j, step, steps
  real(8) :: dx, dt, alpha, r
  real(8), dimension(nx, ny) :: T, T_new
  character(len=100) :: filename
  integer :: ios, dump_interval

  dx            = 0.0025d0          ! 0.25 cm em metros
  alpha         = 1.4d-7
  steps         = 500000
  dt            = 20.0d0/5000      ! = 4e-5, para total = 20 s
  dump_interval = 4000

  r = alpha * dt / (dx*dx)
  print *, 'r = ', r

  ! Condições iniciais e de contorno
  T = 5.0d0
  T(:,1) = 180.0d0;  T(:,ny) = 180.0d0
  T(1,:) = 180.0d0;  T(nx,:) = 180.0d0

  do step = 1, steps
    ! reimpor contorno
    T(:,1) = 180.0d0;  T(:,ny) = 180.0d0
    T(1,:) = 180.0d0;  T(nx,:) = 180.0d0

    ! atualização em diferenças finitas
    do i = 2, nx-1
      do j = 2, ny-1
        T_new(i,j) = T(i,j) + r * ( &
             T(i+1,j) + T(i-1,j) + T(i,j+1) + T(i,j-1) &
           - 4.0d0*T(i,j))
      end do
    end do

    ! reaplicar contorno em T_new
    T_new(:,1) = 180.0d0;  T_new(:,ny) = 180.0d0
    T_new(1,:) = 180.0d0;  T_new(nx,:) = 180.0d0

    T = T_new

    if (mod(step, dump_interval) == 0) then
      write(filename,'(A,I9.9,A)') 'visualization/dump/f_T_',step,'.csv'
      open(unit=10, file=filename, status='replace', action='write', iostat=ios)
      if (ios /= 0) then
        print *, 'Erro ao abrir arquivo: ', filename
        stop
      end if

      do i = 1, nx
        do j = 1, ny
          if (j < ny) then
            write(10,'(F8.3,",")',advance='no') T(i,j)
          else
            write(10,'(F8.3)',       advance='no') T(i,j)
          end if
        end do
        write(10,*)
      end do

      close(10)
      print *, 'Salvo: ', trim(filename)
    end if

  end do

end program difusao_calor