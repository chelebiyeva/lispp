;;;8. Определите функцию, которая разделит исходный список из целых чисел на
;;;два списка: список положительных чисел и список отрицательных чисел.
(defun sol (lst) ((lambda (hl r) (cond
( (null hl) nil )
( (> hl 0) (cons (cons hl (car r)) (list (cadr r))) )
( (< hl 0) (cons (car r) (list (cons hl (cadr r))) ) )
( t r )
)
) (car lst) (cond ((null lst) nil) (t (sol (cdr lst))) ) )
)
;;;(sol '(1 -2 3 0 -4 5 -6))


;;;30. Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную
;;;запись операций в префиксную и возвращает значение выражения. Пример:
;;;> (ВЫЧИСЛИ ’((-2 + 4) * 3))
;;;6
(defun calc (e) (cond
( (atom e) e)
( (null (cadr e)) (list (car e)) )
( t (eval (cons (cadr e) (cons (calc (car e)) (calc (cddr e))))) )
)
)

;;;(calc '((-2 + 4) * 3))



;;;33. Определите список множество, преобразующая список во множество
(defun vxod (x lst)

    (cond

        ((null lst) nil)

        ((equal x (car lst)) (return-from vxod t))

        (t (vxod

 x(cdr lst)))))

(defun spisok (lst)

    ((lambda (head tail) 

       (cond

           ((null lst) nil)

           ((not (vxod head tail)) (cons head (spisok tail))) ;когда голова не входит в хвост, мы ее вносим в результат

           (t (spisok tail))))

        (car lst) (cdr lst)))

;;; (spisok '(1 1 2 1 3 1 2 1 3 3 3 2 1))
;;;(3 2 1)


;;; 11.Определите функцию, осуществляющую разделение исходного списка на два 
;;; подсписка. В первый из них должно попасть указанное количество элементов с начала 
;;;списка, во второй - оставшиеся элементы.

 (defun spl (lst n)	
(cond
    ((< n 0) nil)
    ((null lst) nil)
    ((= n 0) (list () lst))
    (t 
      ((lambda (resault)
        (list
         (cons (car lst) (car resault)) ; добавляет элементы
         (cadr resault))); вывести то, что осталось
         (spl (cdr lst) (- n 1)))
)))
;;; (spl'(1 2 3 4 5 ) 3)
;;;((1 2 3) (4 5))

;;;14. Определите функцию, осуществляющую перестановку двух элементов списка
;;;с заданными номерами.
(DEFUN GET-RANGE-START-END (INPUT-LIST ST END)

  (COND ((ZEROP END) NIL)

        ((AND (= ST 1) (> END 0))

         (CONS (CAR INPUT-LIST)

               (GET-RANGE-START-END (CDR INPUT-LIST) 1 (1- END))))

        (T (GET-RANGE-START-END (CDR INPUT-LIST) (1- ST) END))))
GET-RANGE-START-END

CL-USER 33 : 3 >


(DEFUN SWAP (INPUT-LIST N1 N2)

  (COND ((= N1 N2) INPUT-LIST) ((> N1 N2) (SWAP INPUT-LIST N2 N1))

        (T

         (APPEND (GET-RANGE-START-END INPUT-LIST 1 (- N1 1))

                 (LIST

                  (CAR

                   (GET-RANGE-START-END INPUT-LIST N2

                    (- (LENGTH INPUT-LIST) N2 -1))))

                 (CDR (GET-RANGE-START-END INPUT-LIST N1 (- N2 N1)))

                 (LIST (CAR (GET-RANGE-START-END INPUT-LIST N1 (- N2 N1))))

                 (CDR

                  (GET-RANGE-START-END INPUT-LIST N2

                   (- (LENGTH INPUT-LIST) N2 -1)))))))
SWAP

CL-USER 34 : 3 > (swap `(1 2 3 4 5) 2 4)	
(1 4 3 2 5)


;;;2. Генератор натуральных чисел
(defunget-num ()

    (let ((x 0))

          (lambda () (setq x (+ x 1)))))

(setq g (get-num))

(print 
)


;;;out:1

(print (funcall g))

;;;out:2



;;;#12
;;;Определите функцию, которая возвращает в качестве значения свой вызов.

(defunret-self (args)

    (list 'ret-self args))

(print (ret-self '(123)))

;;;out:(RET-SELF (1 2 3)) 


;;; #1
;;; Определите макрос, который возвращает свой вызов.
(defmacroself (&whole f &rest x) `(quote ,f))
(print (self))
(print (self nil))
(print (self '(1234)))













