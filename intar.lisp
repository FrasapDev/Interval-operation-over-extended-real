; Francesco Saponara    886465

; -*- Mode: Common Lisp -*-
; intar.lisp

; Interval Arithmetic Library ;

;; Constants for infinities and empty interval
  (defconstant +neg-infinity+ '+neg-infinity+)
  (defconstant +pos-infinity+ '+pos-infinity+)
  (defconstant +empty-interval+ ())
  
;; test defined at end of file

(defun empty-interval ()
  +empty-interval+)

(defun extended-real (x)
 (or (eq x +neg-infinity+) (eq x +pos-infinity+) (numberp x)))

(defun is-pos (y)
 (or
    (eq y +pos-infinity+)
    (and (numberp y) (> y 0))
    ))

(defun is-neg (y)
 (or
    (eq y +neg-infinity+)
    (and (numberp y) (< y 0))
    ))

(defun minimum (x &optional y)
  (cond
    ((listp x) 
      (cond
        ((eq y +neg-infinity+) +neg-infinity+)
        ((eq (first x) +neg-infinity+) +neg-infinity+)
        ((and (null y) (extended-real (first x))) 
        (minimum (rest x) (first x)))
        ((null (rest x))
          (cond
            ((eq y +pos-infinity+) (first x))
            ((eq (first x) +pos-infinity+) y)
            ((listp (first x)) (minimum (first x)))
            ((and(numberp (first x)) (< (first x) y)) 
            (first x))
            ((and(numberp (first x)) (> (first x) y)) 
            y)
            ((and(numberp (first x)) (= (first x) y)) 
            y)
            
          )
        )
        
        ((null x) y)
        
        ((eq (first x) +pos-infinity+) (minimum (rest x) y))
        ((eq y +pos-infinity+) (minimum (rest x) (first x)))
        ((and(numberp (first x)) (< (first x) y)) 
        (minimum (rest x) (first x)))
        ((and(numberp (first x)) (> (first x) y)) 
        (minimum (rest x) y))
        ((and(numberp (first x)) (= (first x) y)) 
        (minimum (rest x) y))
        ((and (null y) (listp (first x))) 
        (minimum (list (minimum (first x)) (minimum (rest x)))))
        ((listp (first x)) 
        (minimum (list (minimum (first x)) (minimum (rest x))) y))
      )
    )

  ))

(defun maximum (x &optional y)
  (cond
    ((listp x) 
      (cond
        ((eq y +pos-infinity+) +pos-infinity+)
        ((eq (first x) +pos-infinity+) +pos-infinity+)
        ((and (null y) (extended-real (first x))) 
        (maximum (rest x) (first x)))
        ((null (rest x))
          (cond
            ((eq y +neg-infinity+) (first x))
            ((eq (first x) +neg-infinity+) y)
            ((listp (first x)) (maximum (first x)))
            ((and(numberp (first x)) (> (first x) y)) 
            (first x))
            ((and(numberp (first x)) (< (first x) y)) 
            y)
            ((and(numberp (first x)) (= (first x) y)) 
            y)
          )
        )
        
        ((null x) y)
        ((eq (first x) +neg-infinity+) (maximum (rest x) y))
        ((eq y +neg-infinity+) (maximum (rest x) (first x)))
        ((and(numberp (first x)) (> (first x) y)) 
        (maximum (rest x) (first x)))
        ((and(numberp (first x)) (< (first x) y)) 
        (maximum (rest x) y))
        ((and(numberp (first x)) (= (first x) y)) 
        (maximum (rest x) y))
        ((and (null y) (listp (first x))) 
        (maximum (list (maximum (first x)) (maximum (rest x)))))
        ((listp (first x)) 
        (maximum (list (maximum (first x)) (maximum (rest x))) y))
      )
    )
  ))


(defun +e (&optional x y)
  "Sum over extended reals including infinities."
  (cond
    ;; No arguments returns 0 (unity for addition)
    ((and (null x) (null y)) 0)
    ;; Uniry case
    ((and (extended-real x) (null y)) x)
    ;; Adding infinities
    ((eq x +neg-infinity+)
     (if (eq y +pos-infinity+)
         (error "undefined sum: -infinity + infinity")
         +neg-infinity+))
    ((eq x +pos-infinity+)
     (if (eq y +neg-infinity+)
         (error "undefined sum: infinity + -infinity")
         +pos-infinity+))
    ((eq y +neg-infinity+)
     (if (eq x +pos-infinity+)
         (error "undefined sum: -infinity + infinity")
         +neg-infinity+))
    ((eq y +pos-infinity+)
     (if (eq x +neg-infinity+)
         (error "undefined sum: infinity + -infinity")
         +pos-infinity+))
    ;; Handling numbers
    ((and (numberp x) (numberp y)) (+ x y))
    ;; Default case
    (t (error "Unsupported operation in +e."))))

(defun -e (x &optional y)
  "Subtraction over extended reals."
  (cond
    ;; Unary case (negation)
    ((null y) (if (eq x +pos-infinity+) +neg-infinity+
                (if (eq x +neg-infinity+) +pos-infinity+ (- x))))
    ;; Binary subtraction
    ((and (numberp x) (numberp y)) (- x y))
    ((eq y +neg-infinity+) (+e x +pos-infinity+))
    ((eq y +pos-infinity+) (+e x +neg-infinity+))
    ((eq x +neg-infinity+) (+e +neg-infinity+ y))
    ((eq x +pos-infinity+) (+e +pos-infinity+ y))
    ;; Error for incompatible operations
    (t (error "Unsupported operation in -e."))))

(defun *e (&optional x y)
  "Multiplication over extended reals."
  (cond
    ;; No arguments return 1 (unity for multiplication)
    ((and (null x) (null y)) 1)
    ;; Uniry case
    ((and (extended-real x) (null y)) x)
    ;; Binary case
    ;; 0 * infinities
    ((and (and (numberp x)(zerop x))
    (or (eq y +neg-infinity+)
    (eq y +pos-infinity+)))
    (error "infinities * 0"))
    ;; infinities * 0
    ((and (or (eq x +neg-infinity+) (eq x +pos-infinity+))
     (and (numberp y) (zerop y)))
    (error "infinities * 0"))
    ;; Multiply infinities
    ;; x = + infinity
    ((eq x +pos-infinity+)
          (cond
            ((is-pos y) +pos-infinity+)
            ((is-neg y) +neg-infinity+)
          )
    )
    ;; x = - infinity
    ((eq x +neg-infinity+)
          (cond
            ((is-pos y) +neg-infinity+)
            ((is-neg y) +pos-infinity+)
          )
    )
    ((eq y +pos-infinity+)
          (cond
            ((is-pos x) +pos-infinity+)
            ((is-neg x) +neg-infinity+)
          )
    )
    ((eq y +neg-infinity+)
          (cond
            ((is-pos x) +neg-infinity+)
            ((is-neg x) +pos-infinity+)
          )
    )
    ;; Multiply any number != 0
    ((and (numberp x) (numberp y)) (* x y))

  
    ;; Multiplying numbers

    ;; Default error
    (t (error "Unsupported operation in *e."))))
    ;;(or (cond) (cond)) T

(defun /e (x &optional y)
  "Division over extended reals."
  (cond
    ;; Unary case (reciprocal)
    ((null y) (/e 1 x))
    ;; Binary case
    ;; / 0
    ((and (numberp y) (zerop y)) (error "Division by 0"))
    ;; 0 /
    ((and (numberp x) (zerop x)) 0)
    ;; infinity / infinity
    ((and (or(eq x +pos-infinity+)(eq x +neg-infinity+))
          (or(eq y +pos-infinity+)(eq y +neg-infinity+))
          )(error "infinity / infinity"))
    ;; num / infinity
    ((and (numberp x)
          (or(eq y +pos-infinity+)(eq y +neg-infinity+))
          ) 0)
    ;; infinity / num
    ((eq x +pos-infinity+)
      (cond
        ((is-pos y) +pos-infinity+)
        ((is-neg y) +neg-infinity+)
    ))
    ((eq x +neg-infinity+)
      (cond
        ((is-pos y) +neg-infinity+)
        ((is-neg y) +pos-infinity+)
    ))
    ;; number division
    ((and (numberp x) (numberp y)) (/ x y))

    ;; Default error
    (t (error "Unsupported operation in /e."))))

(defun interval (&optional l h)
  (cond
    ;; No arguments return empty interval
    ((and (null l) (null h)) +empty-interval+)
    ;; Single argument returns singleton interval [l, l]
    ((null h) (if (extended-real l) (list l l)
    (error "Invalid argument: l must be an extended real")))
    ;; Two arguments
    ;; 2 numbers case
    ((and (numberp l) (numberp h))
     (if (<= l h) (list l h) +empty-interval+))
    ;;
    ((and (eq l +neg-infinity+) (extended-real h))
    (list l h))
    ((and (eq h +pos-infinity+) (extended-real h))
    (list l h))
    (t (error "Invalid arguments"))
  ))

(defun whole-interval ()
  "Returns the whole interval IR."
  (list +neg-infinity+ +pos-infinity+))

(defun is-interval (x)
  (cond
    ((and (listp x) (or (listp (first x)) 
    (listp (second x)))) (is-dis-interval x))
    ((and (listp x) (and (and (numberp (first x))
    (numberp (second x))) (<= (first x) (second x)))) T)
    ((and (listp x) (and (eq (first x) +neg-infinity+)
    (extended-real (second x)))) T)
    ((and (listp x) (and (eq (second x) +pos-infinity+)
    (extended-real (first x)))) T)
  ))

(defun is-dis-interval (x)
    (cond
    ((listp (first x))
      (cond
        ((and (is-interval (first x)) 
        (is-interval (list (second (first x)) (second x)))) T)
      )
    )
    ((listp (second x))
      (cond
        ((and (is-interval (second x))
        (is-interval (list (first x) (first (second x))))) T)
      )
    )
    )
)

(defun is-empty (x)
      (cond
        ((or (eq x +empty-interval+)(null x)) T)
        (t (error "Argument is not an interval."))
      )
  )

(defun is-singleton (x)
  "Returns T if x is a singleton interval [z, z]."
  (if (is-interval x)
      (and (listp x) (= (first x) (second x)))
      (error "Argument is not an interval.")))

(defun inf (i)
  "Returns the infimum (lower bound) of the interval i."
  (cond 
    ((null i) (error "Empty interval has no inferior."))
    ((and (is-dis-interval i) (is-interval (first i))) (inf (first i)))
    ((is-interval i) (first i))
    (t (error "Argument is not an interval."))
  )
  )

(defun sup (i)
  "Returns the infimum (lower bound) of the interval i."
  (cond 
    ((null i) (error "Empty interval has no superior."))
    ((and (is-dis-interval i) (is-interval (second i))) (sup (second i)))
    ((is-interval i) (second i))
    (t (error "Argument is not an interval."))
  )
  )

(defun contains (i x)
  "Returns T if interval i contains x (a number or another interval)."
  (cond
    ((not (is-interval i)) (error "First argument is not an interval."))
    ((null x) T) ;; empty interval is always contained

    ((not(is-interval x))
      (cond
        ;; x is an infinity
        ((and (eq x +pos-infinity+) (eq (sup i) +pos-infinity+) ))
        ((and (eq x +neg-infinity+) (eq (inf i) +neg-infinity+) ))
        ;; x is a number
        ((numberp x)
          (cond
            ;; Containment for numbers
            ((and (eq (inf i) +neg-infinity+)
            (eq (sup i) +pos-infinity+)) T)
            ((and (eq (inf i) +neg-infinity+) (numberp (sup i)))
            (<= x (sup i)))
            ((and (numberp (inf i)) (eq (sup i) +pos-infinity+))
            (<= (inf i) x))
            ((and (numberp (inf i)) (numberp (sup i)))
            (and (<= (inf i) x) (<= x (sup i))))
          )
        )
      )
    )
    ;; Containment for intervals
    ((is-interval x) (and (contains i (inf x)) (contains i (sup x))))
    (t (error "Invalid second argument."))))

(defun overlap (i1 i2)
  "Returns T if intervals i1 and i2 overlap."
  (cond
    ((not (is-interval i1)) (error "First argument is not an interval."))
    ((not (is-interval i2)) (error "Second argument is not an interval."))
    ;; Empty intervals don't overlap
    ((or (null i1) (null i2)) nil)
    ;; Check for overlap
    ((or (contains i1 (inf i2)) (contains i1 (sup i2))
         (contains i2 (inf i1)) (contains i2 (sup i1))) t)
    (t nil)))

(defun i+ (&optional x y)
  "Returns the sum of two intervals."
  (cond
    ;; No arguments return singleton interval [0, 0]
    ((and (null x) (null y)) (list 0 0))
    ;; Transform numbers into singleton intervals
    ((extended-real x) (i+ (list x x) y))
    ((extended-real y) (i+ x (list y y)))
    ;; addition over disjoint interval
    ((or (is-dis-interval x) (is-dis-interval y)) 
    (list (i+ (first x) (first y)) (i+ (second x) (second y))))
    ;; addition over standard interval 
    ((and (is-interval x) (is-interval y))
     (list (+e (inf x) (inf y)) (+e (sup x) (sup y))))
    (t (error "Invalid arguments."))))

(defun i- (x &optional y)
  "Returns the subtraction of two intervals."
  (cond
    ((null y) (list (-e (sup x)) (-e (inf x))))
    ((extended-real x) (i- (list x x) y))
    ((extended-real y) (i- x (list y y)))
    ;; subtraction over disjoint interval
    ((or (is-dis-interval x) (is-dis-interval y)) 
    (list (i- (first x) (second y)) (i- (second x) (first y))))
    ;; subtraction over interval
    ((and (is-interval x) (is-interval y))
     (list (-e (inf x) (inf y)) (-e (sup x) (sup y))))
    (t (error "Invalid arguments."))))

(defun i* (&optional x y)
  "Returns the multiplication of two intervals."
  (cond
    ((and (null x) (null y)) (list 1 1))
    ((extended-real x) (i* (list x x) y))
    ((extended-real y) (i* x (list y y)))
    ;; multiplication over disjoint interval
    ((is-dis-interval x) 
      (list
      (minimum (list (i* (first x) y)
                      (i* (second x) y)
                )
        )
      (maximum (list (i* (first x) y)
                      (i* (second x) y)
                )
        )
      )
    )
    ((is-dis-interval y)
      (list
      (minimum (list (i* x (first y))
                      (i* x (second y))
                )
        )
      (maximum (list (i* x (first y))
                      (i* x (second y))
                )
        )
      )
    )
    ;; multiplication over standard interval
    ((and (is-interval x) (is-interval y))
     (list
     (minimum (list (*e (first x) (first y))
                    (*e (first x) (second y))
                    (*e (second x) (first y))
                    (*e (second x) (second y))
              )
      )
     (maximum (list (*e (first x) (first y))
                    (*e (first x) (second y))
                    (*e (second x) (first y))
                    (*e (second x) (second y))
              )
      )
     )
    )
    (t (error "Invalid arguments."))))

(defun i/ (x &optional y)
  "Returns the division of two intervals."
  (cond
    ((null y) (i/ (list 1 1) x))
    ((extended-real x) (i/ (list x x) y))
    ((extended-real y) (i/ x (list y y)))
    ;; division over disjoint interval
    ((is-dis-interval x) 
      (list
      (minimum (list (i/ (first x) y)
                      (i/ (second x) y)
                )
        )
      (maximum (list (i/ (first x) y)
                      (i/ (second x) y)
                )
        )
      )
    )
    ((is-dis-interval y)
      (list
      (minimum (list (i/ x (first y))
                      (i/ x (second y))
                )
        )
      (maximum (list (i/ x (first y))
                      (i/ x (second y))
                )
        )
      )
    )
    ;; division over standard interval
    ((and (is-interval x) (is-interval y))
      (cond
        ;; P1 / P
        ((and (is-pos (first x)) (is-pos (first y)))
        (list (/e (first x) (second y)) (/e (second x) (first y))))
        ((and (is-pos (first x)) (and (numberp (first y))
        (zerop (first y)))) (is-pos (second y))
        (list (/e (first x) (second y)) +pos-infinity+))
        ;; P0 / P
        ((and (and (numberp (first x)) (zerop (first x)))
        (is-pos (second x)) (is-pos (first y)))
        (list 0 (/e (second x) (first y))))
        ((and (and (numberp (first x)) (zerop (first x)))
        (is-pos (second x)) (and (numberp (first y))
        (zerop (first y))) (is-pos (second y)))
        (list 0 +pos-infinity+))
        ;; M / P
        ((and (is-neg (first x)) (is-pos (second x))
        (is-pos (first y)) (is-pos (second y)))
        (list (/e (first x)(first y)) (/e (second x)(first y))))
        ((and (is-neg (first x)) (is-pos (second x))
        (and (numberp (first y)) (zerop (first y))) (is-pos (second y)))
        (list +neg-infinity+ +pos-infinity+))
        ;; N0 / P
        ((and (is-neg (first x)) (and (numberp (second x))
        (zerop (second x))) (is-pos (first y)) (is-pos (second y)))
        (list (/e (first x)(first y)) 0))
        ((and (is-neg (first x)) (and (numberp (second x))
        (zerop (second x))) (and (numberp (first y))
        (zerop (first y))) (is-pos (second y)))
        (list +neg-infinity+ 0))
        ;;N1 / P
        ((and (is-neg (first x)) (is-neg (second x))
        (is-pos (first y)) (is-pos (second y)))
        (list (/e (first x)(first y)) (/e (second x)(second y))))
        ((and (is-neg (first x)) (is-neg (second x))
        (and (numberp (first y)) (zerop (first y))) (is-pos (second y)))
        (list +neg-infinity+ (/e (second x)(second y))))
        ;;P1 / M
        ((and (is-pos (first x)) (is-neg (first y)) (is-pos (second y)))
        (list (list +neg-infinity+ (/e (first x)(first y)))
        (list (/e (first x) (second y)) +pos-infinity+)))
        ;; P0 / M
        ((and (and (numberp (first x)) (zerop (first x)))
        (is-pos(second x)) (is-neg (first y)) (is-pos (second y)))
        (list +neg-infinity+ +pos-infinity+))
        ;; M / M
        ((and (is-neg (first x)) (is-pos(second x)) (is-neg (first y))
        (is-pos (second y))) (list +neg-infinity+ +pos-infinity+))
        ;; N0 / M
        ((and (is-neg (first x)) (zerop(second x)) (is-neg (first y))
        (is-pos (second y))) (list +neg-infinity+ +pos-infinity+))
        ;; N1 / M
        ((and (is-neg (second x)) (is-neg (first y)) (is-pos (second y)))
        (list (list +neg-infinity+ (/e (second x)(second y)))
        (list (/e (second x) (first y)) +pos-infinity+)))
        ;; P1 / N
        ((and (is-pos (first x)) (is-neg (second y))) (list (/e (second x)
        (second y)) (/e (first x) (first y))))
        ((and (is-pos (first x)) (is-neg (first y))
        (and (numberp (second y))
        (zerop (second y))))
        (list +neg-infinity+ (/e (first x) (first y))))
        ;; P0 / N
        ((and (and (numberp (first x)) (zerop (first x)))
        (is-pos (second x)) (is-neg (second y)))
        (list (/e (second x) (second y)) 0))
        ((and (and (numberp (first x)) (zerop (first x)))
        (is-pos (second x)) (is-neg (first y)) (and (numberp (second y))
        (zerop (second y)))) (list +neg-infinity+  0))
        ;; M / N
        ((and (is-neg (first x)) (is-pos (second x)) (is-neg (second y)))
        (list (/e (second x) (second y)) (/e (first x) (second y))))
        ((and (is-neg (first x)) (is-pos (second x)) (is-neg (first y))
        (and (numberp (second y)) (zerop (second y))))
        (list +neg-infinity+  +pos-infinity+))
        ;; N0 / N
        ((and (is-neg (first x)) (and (numberp (second x))
        (zerop (second x))) (is-neg (second y)))
        (list 0 (/e (first x) (second y))))
        ((and (is-neg (first x)) (and (numberp (second x))
        (zerop (second x))) (is-neg (first y))
        (and (numberp (second y)) (zerop (second y))))
        (list 0  +pos-infinity+))
        ;; N1 / N
        ((and (is-neg (first x)) (is-neg (second x))
        (is-neg (second y)))
        (list (/e (second x) (first y)) (/e (first x) (second y))))
        ((and (is-neg (first x)) (is-neg (second x)) (is-neg (first y))
        (and (numberp (second y)) (zerop (second y))))
        (list (/e (second x) (first y))  +pos-infinity+))
      )
    )
    (t (error "Invalid arguments."))))

(defparameter I1 (interval -10 -2))
(defparameter I2 (interval 2 10))
(defparameter I3 (interval +pos-infinity+))
(defparameter I4 (interval +neg-infinity+ +neg-infinity+))
(defparameter I5 (interval +neg-infinity+ -2))
(defparameter I6 (interval 2 +pos-infinity+))
(defparameter I7 (list I1 I2))
(defparameter I8 (list I1 I6))
(defparameter I9 (list I5 I2))
(defparameter I10 (list I5 I6))

;; case test


(defun test-1 ()
  (write-string "TEST OPERATION OVER EXTENDED REAL")
  (terpri)
  (terpri)
  (write-string "1 + 2 = ")
  (write(+e 1 2))
  (terpri)
  (write-string "5 - 2 = ")
  (write (-e 5 2))
  (terpri)
  (write-string "6 * 2 = ")
  (write (*e 6 2))
  (terpri)
  (write-string "9 / 2 = ")
  (write (/e 9 2))
  (terpri)
  (write-string "+infinity + 2 = ")
  (write(+e +pos-infinity+ 2))
  (terpri)
  (write-string "0 - -infinity = ")
  (write (-e 0 +neg-infinity+))
  (terpri)
  (write-string "0 / 2 = ")
  (write (/e 0 2))
  (terpri)
  (terpri)  
  T
)

(defun test-2 ()

  (write-string "TEST DEFINITION OF NEW INTERVALS")
  (terpri)
  (terpri)
  (write-string "I1 = ")
  (write I1)
  (terpri)
  (write-string "I2 = ")
  (write I2)
  (terpri)
  (write-string "I3 = ")
  (write I3)
  (terpri)
  (write-string "I4 = ")
  (write I4)
  (terpri)
  (write-string "I5 = ")
  (write I5)
  (terpri)
  (write-string "I6 = ")
  (write I6)
  (terpri)
  (write-string "I7 = ")
  (write I7)
  (terpri)
  (write-string "I8 = ")
  (write I8)
  (terpri)
  (write-string "I9 = ")
  (write I9)
  (terpri)
  (write-string "I10 = ")
  (write I10)
  (terpri)
  (terpri) 

  T
  )


(defun test-3 ()
  (test-2)
  (write-string "TEST OPERATION OVER INTERVALS")
  (terpri)
  (terpri)
  (write-string "I1 + I5 = ")
  (write(i+ I1 I5))
  (terpri)
  (terpri)
  (write-string "I2 - I6 = ")
  (write(i- I2 I6))
  (terpri)
  (terpri)
  (write-string "I1 * I5 = ")
  (write(i* I1 I5))
  (terpri)
  (terpri)
  (write-string "I2 / I6 = ")
  (write(i/ I2 I6))
  (terpri)
  (terpri)
  T
)

(defun test-4 ()
  (write-string "TEST OPERATION OVER DISJOINT-INTERVALS")
  (terpri)
  (terpri)
  (write-string "I7 + I5 = ")
  (write(i+ I7 I5))
  (terpri)
  (terpri)
  (write-string "I8 - I2 = ")
  (write(i- I8 I2))
  (terpri)
  (terpri)
  (write-string "I7 * I8 = ")
  (write(i* I7 I8))
  (terpri)
  (terpri)
  (write-string "I7 / I9 = ")
  (write(i/ I7 I9))
  (terpri)
  (terpri)

  T
)

(defun all-test ()
  (test-1)
  (test-3)
  (test-4)
)