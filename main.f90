! This file is part of eOn.
!
! SPDX-License-Identifier: BSD-3-Clause
!
! Copyright (c) 2010--present, eOn Development Team
! All rights reserved.
!
! Repo:
! https://github.com/TheochemUI/eOn

program readcon
    use eam_wrap
    use iso_fortran_env, only: real64
    implicit none

    integer :: natms(2), ndim, Nunconstr, Nconstr, Nimage
    integer :: Ncomp, natomslocal
    integer :: i, j, ind, jun
    integer, allocatable :: constr(:)
    real(real64) :: U(1), ax, ay, az, box(3)
    real(real64), allocatable :: R(:), F(:), dum(:,:)
    real(real64) :: matms(2)
    character(128) :: junk

    OPEN(UNIT=101, file='tmp.con', status='old')
    OPEN(UNIT=102, file='forces.out', status='replace')
    OPEN(UNIT=104, file='energy.out', status='replace')
    OPEN(UNIT=103, file='eam.tmp', status='replace')

    read(101,*) junk
    read(101,*) junk
    read(101,*) ax, ay, az
    read(101,*) junk ! we always assume cubic cell
    read(101,*) junk ! no idea what this
    read(101,*) Nconstr, Nunconstr, Nimage
    read(101,*) Ncomp
    read(101,*) natms(1), natms(2)
    read(101,*) matms(1), matms(2)
    natomslocal = natms(1) + natms(2)

    ndim = 3 * natomslocal

    allocate(R(ndim), F(ndim), constr(natomslocal), dum(natomslocal, 3))
    R = 0.0d0
    F = 0.0d0
    ind = 1
    do i = 1, 2
        read(101,*) junk
        read(101,*) junk
        do j = 1, natms(i)
            read(101,*) dum(ind, 1), dum(ind, 2), dum(ind, 3), constr(ind), jun
            ind = ind + 1
        end do
    end do

    ind = 1
    do i = 1, natomslocal
        do j = 1, 3
            R(ind) = dum(i, j)
            ind = ind + 1
        end do
    end do

    box(1) = ax
    box(2) = ay
    box(3) = az

    call c_force_eam(natms, ndim, box, R, F, U)
    write(*,*) 'Energy ', U
    write(*,*) 'Max Force', maxval(F)

    ind = 1
    write(104,*) U(1)
    do i = 1, ndim, 3
        write(102,*) ind, F(i), F(i + 1), F(i + 2)
        ind = ind + 1
    end do

    write(103,*) '1'
end program readcon
