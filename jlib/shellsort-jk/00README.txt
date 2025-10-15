
A shell sort using Tokuda's series 1,4,9,20,46,103 ...
with unknown worst case  performance, but slightly beating builtin SBCL
sort for at least some short values

If done using Pratt's interval series 1,2,3,4,6,8,9,12 with worst case
N [log(N)]^2 performance, it gives worse result than builtin SORT.

see https://en.wikipedia.org/wiki/Shellsort

This is designed as a macro taking a PREDICATE-EXPRESSION and a SWAPPER-EXPRESSION.

The macro design allows full inlining, and can sort on an inclusive
subset of indices ISTART to IEND.   It might beat native SORT when native SORT
would require a :KEY

Using Tokuda, it seems a bit faster than builtin sort, but with no consing, as long
as swap expression is efficient.

Using Pratt's series, it is 2x slower than builtin SORT.  Thus the
default is to use Tokuda.



In 

 (defmacro shellsort-macro (predicate-expression swapper-expression istart iend ivar jvar)

an example usage is 

(defun test-shellsort-macro (vec &key (istart 0) (iend nil))
  (declare (type simple-vector vec))
  (shellsort-macro (< (aref vec i) (aref vec j))        ;; predicate expression
		   (rotatef (aref vec i) (aref vec j))  ;; swapper expression
		   istart                               ;; first index
		   (or iend (1- (length vec)))          ;; last index (inclusive)
		   i j)                                 ;; variables used
  vec) 

There is also a SHELLSORT-FUNCTION, taking predicate and swapper functions instead of 
expressions, but it is about 4x slower than builtin SORT.

