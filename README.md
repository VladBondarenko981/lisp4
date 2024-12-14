<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
</p>
<p align="right">Бондаренко Владислав КВ-13<p>
<p align="right">Рік: 2024<p>

## Завдання

 Завдання складається з двох частин: 
 1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи №3 з такими змінами:
 - використати функції вищого порядку для роботи з послідовностями (де це доречно);
- додати до інтерфейсу в функції (та використання в реалізації) два ключових параметра: key та test, що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому, key має виконатись мінімальну кількість разів.

 2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.


## Варіант 2
 1. Варіант завдання для першої частини обирається той же самий, що і для л.р. №3 (алгоритм сортування обміном №1 (без оптимізацій) за незменшенням).
 2. Варіант завдання для другої частини:
    
 Написати функцію add-prev-reducer , яка має один ключовий параметр — функцію transform. add-prev-reducer має повернути функцію, яка при застосуванні в якості першого аргументу reduce робить наступне: кожен елемент списку-аргументу reduce перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного елемента, а в комірці CDR знаходиться значення попереднього елемента списку (тобто того, що знаходиться "зліва"). Якщо функція transform передана, тоді значення поточного і попереднього елементів, що потраплять у результат, мають бути змінені згідно transform. Обмеження, які накладаються на використання функції-результату add-prev-reducer при передачі у reduce визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції reduce from-end та initial-value ). transform має виконатись мінімальну кількість разів.
 
;Завдання 1
```lisp

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
```


;Завдання 2
```lisp

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
```
