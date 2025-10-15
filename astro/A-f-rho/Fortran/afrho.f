c***file afrho.f
c***karen meech
c***november 2, 2000
c
c - - - Program computes the afrho value for comets.
c
c	Input required in the following form (flush left):
c
c	UTDate    JD     f ap    mag   err   lc   r      d      a    Sun
c	971229  811.7296 V 2.0 21.479 0.014  u  4.482  3.525  3.250 -26.74
c	971229  811.7347 R 2.0 20.989 0.022  u  4.482  3.525  3.250 -27.10
c
c - - - f77 -o afrho afrho.f

	character*77 junk
	character*40 infile, outfile
	character*1 fil, lc

	c = 2.4669e19
	e = 2.71828183
	print 10
10	format('Enter input file: ',$)
        read(*,20) infile
20      format(a)
	print 30
30      format('Enter output file: ',$)
        read(*,20) outfile
	open(unit=3,file=infile,status='old')
	open(unit=4,file=outfile,status='new')
 	read(unit=3,fmt=20) junk
	write(unit=4,fmt=20) junk
40	read(unit=3,fmt=50,end=70) iut,jd,fil,ap,xmag,err,lc,r,d,a,sun
50	format(i6,x,f9.4,x,a1,x,f3.1,x,f6.3,x,f5.3,2x,a1,2x,f5.3,2x,f5.3,x,f6.3,
     1  x,f6.2)
	dm = sun - xmag
	xk = c * r**2.0 * d / (ap * 2)
	afrho = xk * 10**(0.4*dm) 
	aerr = 0.4 / (log10(e)) * afrho *err
	write(unit=4,fmt=60) iut,jd,fil,ap,xmag,err,lc,r,d,a,sun,afrho,aerr
60	format(i6,x,f9.4,x,a1,x,f3.1,x,f6.3,x,f5.3,2x,a1,2x,f5.3,2x,f5.3,x,f6.3,
     1  x,f6.2,x,f6.2,x,f4.2)
	goto 40
70	close(unit=3)
	close(unit=4)
	end
