module euler
contains
  subroutine digits2array(n, array)
    integer, intent(in) :: n
    integer, intent(out), allocatable :: array(:)
    integer :: dignum, d, m, i
    dignum = ceiling(log10(real(n + 1)))
    m = n
    allocate(array(dignum))
    do i=1, dignum
      array(dignum - i + 1) = modulo(m, 10)
      m = m / 10
    end do
  end

  logical function ispalindromic(n)
    integer, intent(in) :: n
    integer, allocatable :: array(:)
    integer :: i, s
    ispalindromic = .true.
    call digits2array(n, array)
    s = size(array)
    do i=1, s
      if (array(i).ne.array(s - i + 1)) then
        ispalindromic = .false.
        return
      end if
    end do
  end function

  logical function ispalindromic2(n)
    integer, intent(in) :: n
    integer :: i, dn, dignum2
    logical :: bitsetat
    dn = dignum2(n)
    ispalindromic2 = .true.
    do i=0, dn / 2
      if (bitsetat(n, i) .neqv. bitsetat(n, dn - i - 1)) then
        ispalindromic2 = .false.
        return
      end if
    end do
  end function

  integer function dignum2(n)
    real :: log2
    integer :: n
    dignum2 = ceiling(log2(real(n + 1)))
  end function

  real function log2(x)
    real, intent(in) :: x
    log2 = log(x) / log(2.)
  end function

  logical function bitsetat(n, k)
    integer, intent(in) :: n, k
    bitsetat = and(n, lshift(1, k)) .ne. 0
  end

end module
