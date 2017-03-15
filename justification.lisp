(defpackage :justify
  (:use :common-lisp :sb-ext)
  (:export #:justify-paragraph))

(in-package :justify)

(defun delimiterp (c) (char= c #\Space))

(defun split (string &key (delimiterp #'delimiterp))
  ;; stolen from stackoverflow
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defvar raw-text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque id nibh sapien. Etiam in tortor malesuada, posuere dolor in, convallis mi. Mauris eu cursus diam. Proin convallis volutpat metus, at imperdiet elit eleifend in. Proin in eros nec quam commodo consequat et sed eros. Mauris eget nulla ex. Morbi ut pharetra magna. Phasellus eleifend orci neque, et pellentesque velit imperdiet vitae. Ut ullamcorper, turpis sit amet ullamcorper iaculis, nisi purus vulputate nisl, et varius libero mauris iaculis velit. Maecenas quis metus et mi blandit maximus. Nunc vitae ultricies massa. Mauris vitae magna sapien.")

(defvar text (split raw-text))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))
              ;; (multiple-value-bind (x y)
              ;;     (apply fn args)
              ;;   (setf (gethash args cache)
              ;;         (list x y))))))))

(defun width (idx jdx n)
  (+ (- jdx idx) ;; spaces
     (loop for string in (subseq text idx (min n (1+ jdx)))
        summing (length string))))

(defun badness (idx jdx n max-width &key (width #'width))
  (let ((w (funcall width idx jdx n)))
    ;; (format t "~&~a~%" w)
    (if (> w max-width)
        most-positive-fixnum
        (expt (- max-width w) 3))))

(defvar *correct* (make-hash-table))

(defun justify-dp (idx max-width)
  (let ((n (length text)))
    (cond ((eql idx n) (list 0 nil))
          (t (let* ((jdx-tbl (make-hash-table))
                    (jdxs (loop for jdx from (1+ idx) to n
                             for res = (+ (badness idx jdx n max-width)
                                          (car (justify-dp jdx max-width)))
                             collect res
                             do (progn (remhash res jdx-tbl)
                                       (setf (gethash res jdx-tbl) jdx))))
                    (min-jdxs (apply #'min jdxs)))
               (list min-jdxs (gethash min-jdxs jdx-tbl)))))))
               ;; (progn (format t "~&~a" (gethash min-jdxs jdx-tbl))
               ;;        min-jdxs))))))
          ;; (t (setf (gethash idx *correct*)
          ;;          (apply #'min
          ;;                 (loop for jdx from (1+ idx) to n
          ;;                    collect (+ (badness idx jdx n max-width)
          ;;                               (justify-dp jdx max-width)))))))))

(setf (fdefinition 'justify-dp) (memoize #'justify-dp))


(defun justified (max-width)
  "List of line beginnings by index of words in TEXT"
  (loop for lb = (justify-dp 0 max-width) then (justify-dp (cadr lb) max-width) until (null (cadr lb)) collecting (cadr lb)))

(defun justify-paragraph (paragraph-text max-width)
  "PARAGRAPH-TEXT is the raw string of paragraph. MAX-WIDTH is the ideal number of columns (monospaced). This is the big enchilada. This brings it all together. Returns a list of lines."
  (let* ((text (split paragraph-text))
         (line-beginnings (justified max-width)))
    (loop for beg-line in (cons 0 line-beginnings)
       for end-line in line-beginnings
       collect (format nil "~{~a~^ ~}" (subseq text beg-line end-line)))))

(defun demo ()
  "This is the proper usage"
  (justify-paragraph raw-text 80))
