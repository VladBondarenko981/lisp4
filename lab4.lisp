;Завдання 1

(defun swap-adjacent (list &key (key #'identity) (test #'<))
  "Обмінює сусідні елементи в ЛІСТі, використовуючи заданий ключ і тест для порівняння."
  (cond
    ((null (cdr list)) list)                       ; Якщо список порожній або має лише один елемент, поверніть його.
    ((funcall test (funcall key (car list)) (funcall key (cadr list))) ; Якщо перший елемент більше за другий, обміняйте їх.
     (cons (car list) (swap-adjacent (cdr list) :key key :test test)))
    (t
     (cons (cadr list) (swap-adjacent (cons (car list) (cddr list)) :key key :test test)))))

(defun bubble-sort-functional (list &key (key #'identity) (test #'<))
  "Сортує ЛІСТ функціональним методом бульбашки, використовуючи заданий ключ і тест для порівняння."
  (let ((swapped-list (swap-adjacent list :key key :test test))) ; Застосовується обмін сусідніх елементів.
    (if (equal swapped-list list)                     ; Якщо більше обмінів не потрібні, поверніть відсортований список.
        list
      (bubble-sort-functional swapped-list :key key :test test)))) ; Інакше, рекурсивно відсортуйте змінений список.

(defun run-bubble-sort-functional-tests ()
  "Запускає тестові випадки для функції bubble-sort-functional."
  (format t "Тест 1: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(3 2 1 0)) '(0 1 2 3)))
  (format t "Тест 2: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(0 1 2 3) :test #'>) '(3 2 1 0)))
  (format t "Тест 3: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(0 1 2 3) :test #'<) '(0 1 2 3)))
  (format t "Тест 4: bubble-sort-functional (порожній список) ~%")
  (format t "~a~%" (equal (bubble-sort-functional '()) '())))

(run-bubble-sort-functional-tests)


;Завдання 2

(defun add-prev-reducer (&key transform)
  "Створює функцію-редуктор, яка додає попереднє значення до кожного елемента в результаті."
  (let* ((transform-fn (or transform #'identity))  ; Використовується функція трансформації за замовчуванням або задана.
         (prev nil))                              ; Ініціалізується попереднє значення.
    (lambda (acc current)
      (let* ((current-val (funcall transform-fn current)) ; Трансформоване поточне значення.
             (prev-val (and prev (funcall transform-fn prev))) ; Трансформоване попереднє значення, якщо є.
             (pair (cons current-val prev-val))) ; Створюється пара (поточне . попереднє).
        (setf prev current)                       ; Оновлюється попереднє значення.
        (nconc acc (list pair))))))              ; Додається пара до результату.

(defun run-add-prev-reducer-tests ()
  "Запускає тестові випадки для add-prev-reducer."
  (format t "Тест 1: add-prev-reducer ~%")
  (format t "~a~%" (equal (reduce (add-prev-reducer) '(1 2 3 4) :from-end nil :initial-value nil) '((1 . NIL) (2 . 1) (3 . 2) (4 . 3))))
  (format t "Тест 2: add-prev-reducer ~%")
  (format t "~a~%" (equal (reduce (add-prev-reducer :transform (lambda (x) (+ x 1))) '(1 2 3 4) :initial-value nil :from-end nil) '((2 . NIL) (3 . 2) (4 . 3) (5 . 4))))
  (format t "Тест 3: add-prev-reducer (порожній список) ~%")
  (format t "~a~%" (equal (reduce (add-prev-reducer) '() :initial-value nil :from-end nil) '())))

(run-add-prev-reducer-tests)
