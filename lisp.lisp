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
