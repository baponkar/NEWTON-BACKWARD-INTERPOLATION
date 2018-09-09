!Newton Backward Interpolation
program nwtn_back
 implicit none
 real::x(0:100),y(0:100),h,s,y_n,del_y(1:100),u,y_0,temp_del,res=0.0
integer::i,j,del_r,del_c,n,f

print*,"How many Data you have"
read*,n

print*,"enter the initial and final values of 'x'"
read*,x(0),x(n-1)

h=(x(n)-x(0))/n

print*,"Now enter the value of 'x' where we gonna finding the valiue of 'y'"
 read*,u

s=(u-x(0))/h

print*,"Now enter the values of Y(x)"

do i=0,n-1
 read*,y(i)
end do

y_0=y(0)

!now we finding the differnce value

do i=1,n-2
 do j=1,n-2
  temp_del=y(j)-y(j-1)
  del_y(i,j)=temp_del
  y(j-1)=temp_del
 end do
end do

!Now calculate the values of the result
i=0
j=n-1

do i=1,n-1
f=f+1
 if(i==n-1)then
  i=1
 else
  i=i+1
 endif

if(j==1)then
 j=n-1
else
j=j-1
endif
print*,del_y(i,j)
res = res+del_y(i,j)*s
s=s*(s+1)/f
end do

print*,"the result is=",res
end program nwtn_back

