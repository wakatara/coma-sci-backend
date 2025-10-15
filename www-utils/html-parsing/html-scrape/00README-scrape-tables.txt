
Routines to parse tables are in scrape-tables.html


For example, this is a two row, three column table.

(defparameter *test-table*
	   "<table>
	       <tr><td>1</td>  <td>2</td>  <td>3</td>  </tr>
	       <tr><td>4</td>  <td>5</td>  <td>6</td>  </tr>
	   </table>")

Then we parse into a toplevel structure into a toplevel tag

(defparameter *top-tag*
	   (cl-html-parse-walker:convert-html-list-to-html-tags
         	      (cl-html-parse:parse-html *test-table*)))

*top-tag* => #S(cl-html-parse-walker:html-tag ....)


Finally, we use this package to parse the table into 2d array that has TABLE-ELEMENT
in the filled sponts, and index-pairs pointing to the parent element where the element
is created through COLSPAN or ROWSPAN

(html-scrape:scrape-html-table-into-array *top-tag*)

or combining into one function:

(defun my-table-parse (html-table-text
                       &key (un-span-elements t)
		            (element-parser-function ;; specify default so we can change it
			      #'html-scrape::default-table-element-parser-function))
   (html-scrape:scrape-html-table-into-array
      (cl-html-parse-walker:convert-html-list-to-html-tags
         	      (cl-html-parse:parse-html html-table-text))
      :un-span-elements un-span-elements
      :element-parser-function element-parser-function))


=================

Example of simple table

 (my-table-parse
	     "<table>
	       <tr><td>1</td>  <td>2</td> <td>3</td> </tr>
	       <tr><td>4</td> <td>5</td> <td>6</td>  </tr>
	   </table>")
==>   #2A((("1") ("2") ("3")) (("4") ("5") ("6")))

Note how each table (array) element is a LIST of the elements in that
table entry.

==================

Example of an html tag <b>hi</b> in 2nd row, first col of table

(my-table-parse
	     "<table>
	       <tr><td>1</td>  <td>2</td> <td>3</td> </tr>
	       <tr><td><b>hi</b></td> <td>5</td> <td>6</td>  </tr>
	   </table>")
#2A((("1") ("2") ("3"))
    ((#S(cl-html-parse-walker:html-tag :name :b :keywords nil :contents ("hi"))) ("5") ("6")))


==================

Example of a COLSPAN, but with UN-SPAN-ELEMENTS=T by default, so
the spanned elements are replaced by original value.

(my-table-parse
	     "<table>
	       <tr><td>1</td>  <td colspan=2>2</td>  </tr>
	       <tr><td>4</td> <td>5</td> <td>6</td>  </tr>
	   </table>")
#2A((("1") ("2") ("2")) (("4") ("5") ("6")))

==================

Example of a COLSPAN, but with UN-SPAN-ELEMENTS=NIL, so there's a TABLE-ELEMENT-REF
pointing to the original element that was spanned.

(my-table-parse
	     "<table>
	       <tr><td>1</td>  <td colspan=2>SP</td>  </tr>
	       <tr><td>4</td> <td>5</td> <td>6</td>  </tr>
	   </table>"
	   :un-span-elements nil)

==> #2A((("1") ("SP") #S(html-scrape::table-element-ref :irow 0 :icol 1))
        (("4") ("5") ("6")))




===================

Example of COLSPAN and ROWSPAN - note how table is expanded with NILs
because the 5,6 entries are pushed right by the ROWSPAN, increasing number of
columns in 2nd row.


(my-table-parse
	     "<table>
	       <tr><td>1</td>  <td colspan=2 rowspan=2>SP</td>  </tr>
	       <tr><td>4</td> <td>5</td> <td>6</td>  </tr>
	   </table>"
	   :un-span-elements t)
	   
==> #2A((("1") ("SP") ("SP")  nil   nil)      ;; SP is colspanned element and NIL is filled in
       (("4") ("SP") ("SP") ("5") ("6")))


==================

Example of using a parser function, assuming each entry is
a string representing an integer, and parsing it.
Note that each entry is always a LIST of html elements, so
a string element is really a list of one string.

(my-table-parse
	     "<table>
	       <tr><td>1</td> <td>2</td> <td>3</td>  </tr>
	       <tr><td>4</td> <td>5</td> <td>6</td>  </tr>
	   </table>"
	   :un-span-elements t
	   :element-parser-function
	      (lambda (content &key irow icol) ;; irow, icol not used but needed
		(declare (ignore irow icol))
		(parse-integer (first content))))

==> #2A((1 3 3)
        (4 5 6))

