subroutine EX_DFFTCF(N, seq, coef)
  !DEC$ ATTRIBUTES DLLEXPORT :: EX_DFFTCF
  !DEC$ ATTRIBUTES C, ALIAS: '_dfftcf' :: EX_DFFTCF
  !DEC$ ATTRIBUTES VALUE :: N
	use imsl
  ! Variables
	integer N
	complex*16 seq(N), coef(N)
  ! Body of EX_DFFTCF
	call DFFTCF(N, seq, coef)
end subroutine

subroutine EX_DFFTCB(N, coef, seq)
  !DEC$ ATTRIBUTES DLLEXPORT:: EX_DFFTCB
  !DEC$ ATTRIBUTES C, ALIAS:'_dfftcb':: EX_DFFTCB
  !DEC$ ATTRIBUTES VALUE :: N
	use imsl
  ! Variables
	integer N, i
	complex*16 seq(N), coef(N)
  ! Body of EX_DFFTCB
	call DFFTCB(N, coef, seq)
end subroutine

subroutine EX_DFFT2D(NRA, NCA, A, LDA, coef, LDCOEF)
  !DEC$ ATTRIBUTES DLLEXPORT:: EX_DFFT2D
  !DEC$ ATTRIBUTES C, ALIAS:'_dfft2d':: EX_DFFT2D
  !DEC$ ATTRIBUTES VALUE :: NRA, NCA, LDA, LDCOEF
	use imsl
  ! Variables
	integer NRA, NCA, LDA, LDCOEF
	complex*16 A(NRA, NCA), coef(NRA, NCA)
  ! Body of EX_DFFT2D
	call ArrayC2For(NRA, NCA, A)
	call DFFT2D(NRA, NCA, A, LDA, coef, LDCOEF)
	call ArrayFor2C(NRA, NCA, coef)
end subroutine

subroutine EX_DFFT2B(NRCOEF, NCCOEF, coef, LDCOEF, A, LDA)
  !DEC$ ATTRIBUTES DLLEXPORT:: EX_DFFT2B
  !DEC$ ATTRIBUTES C, ALIAS:'_dfft2b':: EX_DFFT2B
  !DEC$ ATTRIBUTES VALUE :: NRCOEF, NCCOEF, LDCOEF, LDA
	use imsl
  ! Variables
	integer NRCOEF, NCCOEF, LDCOEF, LDA
	complex*16 coef(NRCOEF, NCCOEF), A(NRCOEF, NCCOEF)
  ! Body of EX_DFFT2B
	call ArrayC2For(NRCOEF, NCCOEF, coef)
	call DFFT2B(NRCOEF, NCCOEF, coef, LDCOEF, A, LDA)
	call ArrayFor2C(NRCOEF, NCCOEF, A)
end subroutine

subroutine EX_DRCORL(IDO, N, X, Y, IPAD, NZ, Z, ZHAT)
  !DEC$ ATTRIBUTES DLLEXPORT:: EX_DRCORL
  !DEC$ ATTRIBUTES C, ALIAS:'_drcorl':: EX_DRCORL
  !DEC$ ATTRIBUTES VALUE :: IDO, N, IPAD
	use imsl
  ! Variables
	integer IDO, N, IPAD, NZ
	real*8 X(N), Y(N), Z(NZ), ZHAT(NZ)
  ! Body of EX_DRCORL
	call DRCORL(IDO, N, X, Y, IPAD, NZ, Z, ZHAT)
end subroutine

subroutine ArrayC2For(NR, NC, Array)
  ! Variables
	integer NR, NC
	complex*16 Array(NR*NC)
	complex*16, allocatable:: temp(:)
	integer i, j
  ! Body of	ArrayC2For
	allocate(temp(NR*NC))
	do i=1, NR
		do j=1, NC
			temp(NR*(j-1)+i) = Array(NC*(i-1)+j)
		end do
	end do

	do i=1, NR*NC
		Array(i) = temp(i)
	end do
	deallocate(temp)
end subroutine

subroutine ArrayFor2C(NR, NC, Array)
  ! Variables
	integer NR, NC
	complex*16 Array(NR*NC)
	complex*16, allocatable:: temp(:)
	integer i, j
  ! Body of	ArrayC2For
	allocate(temp(NR*NC))
	do j=1, NC
		do i=1, NR
			temp(NC*(i-1)+j) = Array(NR*(j-1)+i)
		end do
	end do

	do i=1, NR*NC
		Array(i) = temp(i)
	end do
	deallocate(temp)
end subroutine
