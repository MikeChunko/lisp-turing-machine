;;; tm.lisp
;;; Simulate a Turing machine

(defun tm (input_tape transitions)
  "Simulate a Turing machine.
   Transitions should be an a-list of the form
   '((state (symbol (new_state, action)) ... ) ... )
   Any NIL transitions will be interpreted as a halt, upon which
   the tape will be output."

  (defun read_head (right_tape)
    "Read the head of the turing machine"
    (car right_tape))

  (defun write_head (right_tape symbol)
    "Write to the input tape at head's location"
    (cons symbol (cdr right_tape)))

  (defun move_right (left_tape right_tape)
    "Simulate a moving the head right"
    (if (eq nil right_tape)
        (values left_tape right_tape)
        (values
          (cons (read_head right_tape) left_tape)
          (cdr right_tape))))

  (defun move_left (left_tape right_tape)
    "Simulate moving the head left"
    (if (eq nil left_tape)
        (values left_tape right_tape)
        (values
          (cdr left_tape)
          (cons (read_head left_tape) right_tape))))

  (defun tm_helper (left_tape right_tape state)
    (princ "STEP: ")
    (write state)
    (terpri)
    (write left_tape)
    (princ ", ")
    (write right_tape)
    (terpri) (terpri)

    (setf transition (cadr (assoc (read_head right_tape) (cdr (assoc state transitions)))))
    (setf new_state (car transition))
    (setf action (cadr transition))

    (cond
      ((eq action 'LEFT)
        (multiple-value-bind
          (left_tape right_tape)
          (move_left left_tape right_tape)
        (tm_helper left_tape right_tape new_state)))
      ((eq action 'RIGHT)
        (multiple-value-bind
          (left_tape right_tape)
          (move_right left_tape right_tape)
        (tm_helper left_tape right_tape new_state)))
      ((eq action 'HALT)
        (write left_tape)
        (terpri)
        (write right_tape)
        (terpri))
      ((eq action NIL)
        (write left_tape)
        (terpri)
        (write right_tape)
        (terpri))
      (t
        (tm_helper left_tape (write_head right_tape action) new_state))))

  (tm_helper () input_tape 0))

(tm '(1 0 1 Y 1 1) '((0
                      (1 (1 LEFT))
                      (2 (1 a)
                      (3 (0 NIL))))))
