(defun load-swisseph ()
 (cffi:load-foreign-library "~/trading/lisp/libswe.so"))

(cffi:defcfun ("swe_set_ephe_path" set-ephe-path) :void (path :string))
(cffi:defcfun ("swe_set_jpl_file" set-jpl-file) :void (fname :string))
(cffi:defcfun ("close" close-swe) :void)

;; We need to ensure a 255 byte char * is passed in
(cffi:defcfun ("swe_version" version) :string (svers :string))
(cffi:defcfun ("swe_get_library_path" get-library-path) :string (spath :string))
(cffi:defcfun ("swe_get_planet_name" get-planet-name) :string (ipl :int) (spname :string))

(cffi:defcstruct (c-results :class c-results)
  (long :double)
  (lat :double)
  (dist :double)
  (speed-long :double)
  (speed-lat :double)
  (speed-dist :double))

(defstruct results
  (long 0d0 :type double-float)
  (lat 0d0 :type double-float)
  (dist 0d0 :type double-float)
  (speed-long 0d0 :type double-float)
  (speed-lat 0d0 :type double-float)
  (speed-dist 0d0 :type double-float))

(defmethod translate-from-foreign (ptr (type c-results))
  (cffi:with-foreign-slots ((long lat dist speed-long speed-lat speed-dist) ptr (:struct results))
    (make-results :long long :lat lat :dist dist
		  :speed-long speed-long :speed-lat speed-lat
		  :speed-dist speed-dist)))

(defmethod expand-from-foreign (ptr (type c-results))
  `(cffi:with-foreign-slots ((long lat dist speed-long speed-lat speed-dist) ,ptr (:struct results))
     (make-results :long long :lat lat :dist dist
		  :speed-long speed-long :speed-lat speed-lat
		  :speed-dist speed-dist)))

(defmethod translate-into-foreign-memory (value (type c-results) ptr)
    (cffi:with-foreign-slots ((long lat dist speed-long speed-lat speed-dist) ptr (:struct results))
      (setf long (results-long value)
	    lat (results-lat value)
	    dist (results-dist value)
	    speed-long (results-speed-long value)
	    speed-lat (results-speed-lat value)
	    speed-dist (results-speed-dist value))))

(cffi:defcfun ("swe_calc_ut" %calc-ut) :int
  (tjd_ut :double)
  (ipl :int)
  (iflag :int)
  (xx :pointer (:struct :results))
  (serr :string))

(cffi:defcfun ("swe_calc" %calc) :int (tjd_ut :double) (ipl :int) (iflag :int) (xx :pointer :double) (serr :string))

;; Also need to do
;; swe_fixstar2_ut(), swe_fixstar2(), swe_fixstar_ut(),  swe_fixstar()
;; Time conversion functions

(defun make-strmax ()
  (make-string 255 :initial-element #\Space))

(defmacro build-single (fname wrapped)
  "Creates a single-use function which wraps the given function, freeing the
   necessary memory structure after and converting to a struct"
  `(defun ,fname (tjd-ut planet iflag)
     (let* ((xx (translate-into-foreign-memory ))
	    (serr (make-strmax)))
       (unwind-protect
	    (let ((rescode (,wrapped tjd-ut planet iflag xx serr)))
	      (if (< rescode 0)
		  (error (format nil "Swisseph reported an error, description is: ~S" serr))
		  (translate-from-foreign xx))))
       (cffi:foreign-free xx)))))

(build-single calc %calc)
(build-single calc-ut %calc-ut)

(defconstant ECL_NUT       -1)
(defconstant SUN            0)
(defconstant MOON           1)
(defconstant MERCURY        2)
(defconstant VENUS          3)
(defconstant MARS           4)
(defconstant JUPITER        5)
(defconstant SATURN         6)
(defconstant URANUS         7)
(defconstant NEPTUNE        8)
(defconstant PLUTO          9)
(defconstant MEAN_NODE     10)
(defconstant TRUE_NODE     11)
(defconstant MEAN_APOG     12)
(defconstant OSCU_APOG     13)
(defconstant EARTH         14)
(defconstant CHIRON        15)
(defconstant PHOLUS        16)
(defconstant CERES         17)
(defconstant PALLAS        18)
(defconstant JUNO          19)
(defconstant VESTA         20)
(defconstant INTP_APOG     21)
(defconstant INTP_PERG     22)
(defconstant NPLANETS      23)
(defconstant FICT_OFFSET   40)
(defconstant NFICT_ELEM    15)
(defconstant AST_OFFSET    10000)
 
;; Hamburger or Uranian "planets" */
(defconstant CUPIDO        40)
(defconstant HADES         41)
(defconstant ZEUS          42)
(defconstant KRONOS        43)
(defconstant APOLLON       44)
(defconstant ADMETOS       45)
(defconstant VULKANUS      46)
(defconstant POSEIDON      47)

;; Other fictitious bodies 
(defconstant ISIS               48)
(defconstant NIBIRU             49)
(defconstant HARRINGTON         50)
(defconstant NEPTUNE_LEVERRIER  51)
(defconstant NEPTUNE_ADAMS      52)
(defconstant PLUTO_LOWELL       53)
(defconstant PLUTO_PICKERING    54)
