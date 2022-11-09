(defvar *array* (loop for i from 0 to 100 collect 0))
(defvar *index* 0)

(defun bracket ( op code direction )
  (let ((ch #\Space))
    (let ((bracket-count 1))
    (progn
        (loop while (/= bracket-count 0)
              do (progn
                    (setf op (+ op direction))
                    (setf ch (char code op))
                    (cond 
                      ((char= ch #\[)
                        (setf bracket-count (+ bracket-count direction)))
                      ((char= ch #\])
                        (setf bracket-count (- bracket-count direction))))))
        op))))

(defun bf-op (op-index c code)
  (cond
    ((string= c ">") (progn
                       (incf *index*)
                       (incf op-index)))

    ((string= c "<") (progn 
                       (decf *index*)
                       (incf op-index)))

    ((string= c "+") (progn 
                       (incf (nth *index* *array*))
                       (incf op-index)))

    ((string= c "-") (progn
                       (decf (nth *index* *array*))
                       (incf op-index)))

    ((string= c "[") (if (eq (nth *index* *array*) 0)
                       (bracket op-index code 1)
                       (incf op-index)))

    ((string= c "]") (if (eq (nth *index* *array*) 0)
                       (incf op-index)
                       (bracket op-index code -1)))

    ((string= c ".") (progn
                       (format t "~c" 
                               (code-char 
                                 (nth *index* *array*)))
                       (incf op-index)))

    ((string= c ",") (progn
                       (let ((x 0))
                         (setf x 
                               (char-code (read-char)))
                         (setf 
                            (nth *index* *array*) x))
                       (incf op-index)))

    (t (incf op-index))))

(defun bf-eval (code)
  (let ((op 0))
    (loop while (< op (length code))
        do (setf op (bf-op op (char code op) code)))))

; működő hello world
(bf-eval "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
; és cat programok
(bf-eval ",[.,]")
