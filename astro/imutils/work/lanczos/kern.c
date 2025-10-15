/* Lanczos kernel code adapted from SWARP interpolate.c to test our coefficent generation 

License:		GNU General Public License

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define PI 3.141592654

void lanc2(double pos, double *kernel)
{
  double x, val, sinx1,sinx2,sinx3,cosx1;
  for (int i=0; i<4; i++) kernel[i]=0.0;
  x = -PI/2.0*(pos+1.0);
  sinx1 = sin(x);
  cosx1 = cos(x);
  val = (*(kernel++) = sinx1/(x*x));
  x += PI/2.0;
  val += (*(kernel++) = -cosx1/(x*x));
  x += PI/2.0;
  val += (*(kernel++) = -sinx1/(x*x));
  x += PI/2.0;
  val += (*kernel = cosx1/(x*x));
  val = 1.0/val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *kernel *= val;
}


void lanc3(double pos, double *kernel)
{
  double x, val, sinx1,sinx2,sinx3,cosx1;
  for (int i=0; i<6; i++) kernel[i]=0.0;
  x = -PI/3.0*(pos+2.0);
  sinx1 = sin(x);
  cosx1 = cos(x);
  // c1
  val = (*(kernel++) = sinx1/(x*x));
  x += PI/3.0;
  // c2
  val += (*(kernel++) = (sinx2=-0.5*sinx1-0.866025403785*cosx1)
	  / (x*x));
  x += PI/3.0;
  // c3
  val += (*(kernel++) = (sinx3=-0.5*sinx1+0.866025403785*cosx1)
	  /(x*x));
  x += PI/3.0;
  // c4
  val += (*(kernel++) = sinx1/(x*x));
  x += PI/3.0;
  // c5
  printf("sinx2=%f   x=%f\n", sinx2, x);
  val += (*(kernel++) = sinx2/(x*x));
  x += PI/3.0;
  // c6
  val += (*kernel = sinx3/(x*x));
  val = 1.0/val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *(kernel--) *= val;
  *kernel *= val;
}



int main (int argc, char **argv)
{
  double pos = atof(argv[1]);
  double kern[10];
  int i;

  lanc2(pos,kern);
  printf("Lancoz2 coefficents for pos=%f\n",pos);
  for (i=0; i<4; i++)
    printf("  c%d = %f\n",i+1, kern[i]);

  printf("\n\n");
  
  lanc3(pos,kern);
  printf("Lancoz3 coefficents for pos=%f\n",pos);
  for (i=0; i<6; i++)
    printf("  c%d = %f\n",i+1, kern[i]);
  
  return 0;
}
