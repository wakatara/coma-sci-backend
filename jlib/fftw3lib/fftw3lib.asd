

(asdf:defsystem fftw3lib
  :depends-on (bordeaux-threads)
  :components
  ((:file "fftw3-package" :depends-on ())
   (:file "fftw3-ffi" :depends-on ("fftw3-package"))
   (:file "fftw3-plans" :depends-on ("fftw3-package" "fftw3-ffi"))
   (:file "transform-c2c" :depends-on ("fftw3-package" "fftw3-ffi" "fftw3-plans"))
   (:file "one-d-transforms" :depends-on ("fftw3-package" "fftw3-ffi"
							  "fftw3-plans"))
   (:file "utils" :depends-on ("fftw3-package"))
   
   ))
