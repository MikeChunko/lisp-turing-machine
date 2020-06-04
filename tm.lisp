;;; tm.lisp
;;; Simulate a Turing machine

(defun tm (input_tape transitions)
    "Simulate a Turing machine"

    (defun read_head (right_tape)
        (car right_tape))

    (defun move_right (left_tape right_tape)
        (values
            (cons (read_head right_tape) left_tape)
            (cdr right_tape)))

    (defun move_left (left_tape right_tape)
        (values
            (cdr left_tape)
            (cons (read_head left_tape) right_tape)))

    (defun tm_helper (left_tape right_tape state)
        (multiple-value-bind
            (left_tape right_tape)
            (move_right (list 3 2 1) (list 4 5 6))
        (write left_tape)
        (terpri)
        (write right_tape)
        (terpri)
        (read_head right_tape)))

    (tm_helper () input_tape 0))

(write (tm 0 0))
