      EXTERNAL FEX, GEX
      DOUBLE PRECISION ATOL, RTOL, RWORK, T, TOUT, Y, tfinal, event
      DOUBLE PRECISION, PARAMETER :: GAMMA = 0.004, TOE = 1d-12
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, JT,
     1  NG, JROOT
      INTEGER num_steps
      DIMENSION Y(4), ATOL(4), RWORK(89), IWORK(24), JROOT(1)
      ! RWORK >= 22 + NEQ * MAX(16, NEQ + 9) + 3*NG
      ! IWORK >= 20 + NEQ
      NEQ = 4
      Y(1) =  0.150         ! theta (rad)
      Y(2) =  0.310         ! phi (rad)
      Y(3) = -0.150         ! thetadot (rad/s)
      Y(4) = -0.007         ! phidot (rad/s)
      T = 0.0               ! initial time point
      TOUT = 0.005          ! first time point output is desired
      tfinal = 4.5
      num_steps = CEILING((tfinal-T)/TOUT)
      ITOL = 2
      RTOL = 1.d-9
      ATOL(1) = 1.d-9
      ATOL(2) = 1.d-9
      ATOL(3) = 1.d-9
      ATOL(4) = 1.d-9
      ITASK = 1             !  1 for normal computation of output values
      ISTATE = 1            !  1 = initial state flag
                            !  2 = no root found, continue normally
                            !  3 = root was found prior to reaching tout
                            ! -x = error, see documentation
      IOPT = 0              !  0 = no optional inputs used
      LRW = 89              !    = RWORK
      LIW = 24              !    = IWORK
      JT = 2                !    = initial value
      NG = 1                !    = number of event functions
!      DO 40 IOUT = 1,num_steps
!  10    CALL DLSODAR(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
!     1     IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT,GEX,NG,JROOT)
!        event = Y(2) - 2*(Y(1))
!        WRITE(6,20)T,Y(1),Y(2),Y(3),Y(4),event
!  20    FORMAT(' At t =',f12.8,'      Y =',4f14.9,'  event =',f14.9)
!        IF (ISTATE .LT. 0) GO TO 80
!        IF (ISTATE .EQ. 2) GO TO 40
!        WRITE(6,30)JROOT(1)
!  30    FORMAT(5X,' The above line is a root,  JROOT =',I5)
!        ISTATE = 2
!        GO TO 10
!  40    TOUT = TOUT + 0.005     ! end of DO loop
!      WRITE(6,60)IWORK(11),IWORK(12),IWORK(13),IWORK(10),
!     1   IWORK(19),RWORK(15)
!  60  FORMAT(/' No. steps =',I4,'  No. f-s =',I4,'  No. J-s =',I4,
!     1   '  No. g-s =',I4/
!     2   ' Method last used =',I2,'   Last switch was at t =',f12.9)
!      STOP
!  80  WRITE(6,90)ISTATE
!  90  FORMAT(///' Error halt.. ISTATE =',I3)
!      STOP
!      END


      DO 40 IOUT = 1,num_steps
  10    CALL DLSODAR(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     1     IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT,GEX,NG,JROOT)
        event = Y(2) - 2*(Y(1))
        WRITE(6,20)T,Y(1),Y(2),Y(3),Y(4),event
  20    FORMAT(' At t =',f12.8,'      Y =',4f14.9,'  event =',f14.9)
        IF (ISTATE .LT. 0) GO TO 80
        IF (ISTATE .EQ. 2) GO TO 40
        WRITE(6,30)JROOT(1)
  30    FORMAT(5X,' The above line is a root,  JROOT =',I5)
        ! Terminate Integration Upon Finding a Particular Root?
        EVENT_SLOPE = Y(4) - 2*Y(3)     ! direction of zero crossing
        IF (EVENT_SLOPE .LT. 0) THEN    ! don't stop integrating
            ISTATE = 2
            GO TO 10
        ELSEIF (EVENT_SLOPE .GT. 0) THEN
            print*,'     Zero Crossing Found. Stopping Integration.'
            EXIT
        END IF
  40    TOUT = TOUT + 0.005     ! end of DO loop
      WRITE(6,60)IWORK(11),IWORK(12),IWORK(13),IWORK(10),
     1   IWORK(19),RWORK(15)
  60  FORMAT(/' No. steps =',I4,'  No. f-s =',I4,'  No. J-s =',I4,
     1   '  No. g-s =',I4/
     2   ' Method last used =',I2,'   Last switch was at t =',f12.9)
      STOP
  80  WRITE(6,90)ISTATE
  90  FORMAT(///' Error halt.. ISTATE =',I3)
      STOP
      END




      SUBROUTINE FEX (NEQ, T, Y, YDOT)
      DOUBLE PRECISION T, Y, YDOT
      DOUBLE PRECISION, PARAMETER :: GAMMA = 0.004
      DIMENSION Y(4), YDOT(4)
      ! YDOT = [thetadot, phidot, thetaddot, phiddot]
      YDOT(1) = Y(3)
      YDOT(2) = Y(4)
      YDOT(3) = sin(Y(1) - GAMMA)
      YDOT(4) = sin(Y(1)-GAMMA) + sin(Y(2))*(Y(3)**2 - cos(Y(1)-GAMMA))
      RETURN
      END

      SUBROUTINE GEX (NEQ, T, Y, NG, GOUT)
      DOUBLE PRECISION T, Y, GOUT
      INTEGER NEQ, NG
      DIMENSION Y(4), GOUT(1)
      GOUT(1) = Y(2) - 2*Y(1)       ! phi - 2*theta = 0
      RETURN
      END
