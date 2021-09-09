c Generalized for any-bit machine: Paul Harasti 6/2012
c
c#####################################################################
      subroutine print_date_time(yyyymmddhhmmss,data_year,data_month
     &                          ,data_day,data_hour,data_minute
     &                          ,data_second)
c#####################################################################
c********0*********0*********0*********0*********0*********0*********
c
      implicit none
c
c********0*********0*********0*********0*********0*********0*********
      integer*4 data_year
      integer*4 data_month
      integer*4 data_day
      integer*4 data_hour
      integer*4 data_minute
      integer*4 data_second

      character*14 yyyymmddhhmmss
c********0*********0*********0*********0*********0*********0*********
      read(yyyymmddhhmmss(1:4),60)data_year
   60 format(i4)
      read(yyyymmddhhmmss(5:6),70)data_month
   70 format(i2)
      read(yyyymmddhhmmss(7:8),70)data_day
      read(yyyymmddhhmmss(9:10),70)data_hour
      read(yyyymmddhhmmss(11:12),70)data_minute
      read(yyyymmddhhmmss(13:14),70)data_second
c     print *,'data year=',data_year
c     print *,'data month=',data_month
c     print *,'data day=',data_day
c     print *,'data hour=',data_hour
c     print *,'data minute=',data_minute
c     print *,'data second=',data_second
c********0*********0*********0*********0*********0*********0*********
      return
      end
