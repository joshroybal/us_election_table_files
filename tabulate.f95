program election_data
   implicit none
   ! interface declaration
   interface
      ! function constructs and returns header row
      function construct_header(flag) result(header)
         character (len=*), intent(in) :: flag
         character (len=256) :: header
      end function construct_header
      ! function constructs and returns an individual record row
      function construct_record(label, votes, pcts, flag) result(record)
         character (len=*), intent(in) :: label, flag
         integer, dimension(8), intent(in) :: votes
         real, dimension(3), intent(in) :: pcts
         character (len=256) :: record
      end function construct_record
   end interface
   ! data declarations
   integer, parameter :: NCOLS=52
   integer :: eof, i, j, n
   character (len=4) :: flag
   character (len=256) :: header
   character (len=NCOLS) :: infile, ofile
   integer, dimension(6,NCOLS) :: itab
   integer, dimension(8,NCOLS) :: otab
   real, dimension(3,NCOLS) :: pcts
   character (len=14), dimension(NCOLS) :: labels
   character (len=256), dimension(NCOLS) :: record
   ! processing
   call get_command_argument(1,infile)
   call get_command_argument(2,flag)

   if (command_argument_count() < 2) then
      print *, 'Usage: tabulate filename csv|tab|flat|html'
      stop 'processing terminated'
   end if
   if (flag /= 'csv' .and. flag /= 'tab' .and. flag /= 'flat' &
   .and. flag /= 'html') stop 'Usage: ./tabulate filename csv|tab|flat|html'
   n=1
   open (7,file=infile,status='old',access='direct',form='formatted',recl=44)
   ! input block
   do
      read (7,1000,rec=n,iostat=eof) labels(n), itab(1:3,n), itab(4:6,n)
      1000 format (a14,3i2,3i8)
      if (eof /= 0) exit
      n = n + 1
   end do
   close (7)
   ! core processing block
   n = n - 1! tag highest value of n
   labels(n+1) = 'U. S. Total'
   do j = 1, n
      otab(1,j) = itab(3,j)
      otab(2:3,j) = itab(1:2,j)
      otab(4,j) = itab(3,j)-sum(itab(1:2,j))
      otab(5:6,j) = itab(4:5,j)
      otab(7,j) = itab(6,j)-sum(itab(4:5,j))
      !otab(8,j) = sum(otab(5:7,j))
      otab(8,j) = itab(6,j)
      ! n + 1 column is U. S. Total record
      otab(1:8,n+1) = otab(1:8,n+1) + otab(1:8,j)
   end do
   do j = 1, n
      do i = 1, 3
         pcts(i,j) = 100.*otab(i+4,j)/itab(6,j)
      end do
   end do
   ! U. S. Total %
   do i = 1, 3
      pcts(i,n+1) = 100.*otab(i+4,n+1)/otab(8,n+1)
   end do
   ! convert tables to record strings
   header = construct_header(flag)
   do j=1,n+1
      record(j) = construct_record(labels(j), otab(1:8,j), pcts(1:3,j), flag)
   end do
   ! output block
   if (flag == 'flat') then
      ofile='reports/'
   else
      ofile=trim(flag)//'/'
   end if
   ofile=trim(ofile)//infile(8:11)
   if (flag == 'csv') then
      ofile=trim(ofile)//'.csv'
   else if (flag == 'html') then
      ofile=trim(ofile)//'.html'
   else
      ofile=trim(ofile)//'.txt'
   end if
   open (8,file=ofile,status='unknown')
   if (flag == 'html') then
      write (8,'(a)') '<!DOCTYPE html>'
      write (8,'(a)') '<html>'
      write (8,'(a)') '<head>'
      write (8,'(a)') '<link id="styleinfo" media="all">'
      write (8,'(a)') '<script type="text/javascript" src="style.js" defer></script>'
      write (8,'(a)') '</head>'
      write (8,'(a)') '<body>'
      write (8,'(a)') '<table id="my_table">'
   end if
   write (8,'(a)') trim(header)
   do j=1,n+1
      write (8,'(a)') trim(record(j))
   end do
   if (flag == 'html') then
      write (8,'(a)') '</table>'
      write (8,'(a)') '</body>'
      write (8,'(a)') '</html>'
   end if
   close (8)
   print *, 'processing terminated - output dumped to ', trim(ofile)
end program election_data

! function constructs and returns header row
function construct_header(flag) result(header)
   implicit none
   ! dummy arguments
   character (len=*), intent(in) :: flag
   ! result location
   character (len=256) :: header
   ! local data
   integer :: i, n
   character (len=11), dimension(11):: header_list
   data header_list/'EC','D EC','R EC','I EC','D #','R #','I #','TOTAL','D %',&
      'R %','I %'/
   ! processing
   header = '' ! clear any junk
   if (flag == 'flat') then
      header(1:14) = 'STATE'
      n = 15
      do i = 1, 4
         header(n:n+3) = adjustr(header_list(i)(1:4))
         n = n + 4
      end do
      do i = 5, 8
         header(n:n+9) = adjustr(header_list(i)(1:10))
         n = n + 10
      end do
      do i = 9, 11
         header(n:n+7) = adjustr(header_list(i)(1:8))
         n = n + 8
      end do
      return
   else if (flag == 'html') then 
      header='<tr><th>STATE</th>'
   else
      header='STATE'
   end if
   do n=1,11
      if (flag == 'csv') header=trim(header)//','
      if (flag == 'tab') header=trim(header)//char(9)
      if (flag == 'html') header=trim(header)//'<th>'
      header = trim(header)//header_list(n)
      if (flag == 'html') header=trim(header)//'</th>'
   end do
   if (flag == 'html') header=trim(header)//'</tr>'
end function construct_header

! function constructs and returns an individual record row
function construct_record(label, votes, pcts, flag) result(record)
   implicit none
   ! dummy arguments
   character (len=*), intent(in) :: label, flag
   integer, dimension(8), intent(in) :: votes
   real, dimension(3), intent(in) :: pcts
   ! function return location
   character (len=256) :: record
   ! local data
   integer i, n
   character (len=25) :: field
   ! processing
   record = '' ! clear any junk
   if (flag == 'flat') then
      record(1:14) = label
      n = 15
      do i = 1, 4
         write (record(n:n+3),'(i4)') votes(i)
         n = n + 4
      end do
      do i = 5, 8
         write (record(n:n+9),'(i10)') votes(i)
         n = n + 10
      end do
      do i = 1, 3
         write (record(n:n+7),'(f8.2)') pcts(i)
         n = n + 8
      end do
      return
   else if (flag == 'html') then 
      record='<tr><td>'//trim(label)//'</td>'
   else
      record=trim(label)
   end if
   do i = 1, 8
      if (flag == 'csv') record = trim(record)//','
      if (flag == 'tab') record = trim(record)//char(9)
      if (flag == 'html') record = trim(record)//'<td>'
      write (field,*) votes(i)
      record = trim(record)//adjustl(field)
      if (flag == 'html') record = trim(record)//'</td>'
   end do
   do i = 1, 3
      if (flag == 'csv') record = trim(record)//','
      if (flag == 'tab') record = trim(record)//char(9)
      if (flag == 'html') record = trim(record)//'<td>'
      write (field,'(f8.2)') pcts(i)
      record = trim(record)//adjustl(field)
      if (flag == 'html') record = trim(record)//'</td>'
   end do   
   if (flag == 'html') record = trim(record)//'</tr>'
end function construct_record
