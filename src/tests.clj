(require '[clojure.test :refer [is deftest run-tests]])

(load-file "src/basic.clj")

(deftest test-palabra-reservada?
  (is (= true (palabra-reservada? 'REM)))
  (is (= false (palabra-reservada? 'SPACE)))
  (is (= true (palabra-reservada? 'NEW)))
  (is (= true (palabra-reservada? 'CLEAR)))
  (is (= true (palabra-reservada? 'LIST)))
  (is (= true (palabra-reservada? 'RUN)))
  (is (= true (palabra-reservada? 'LOAD)))
  (is (= true (palabra-reservada? 'SAVE)))
  (is (= true (palabra-reservada? 'LET)))
  (is (= true (palabra-reservada? 'INT)))
  (is (= true (palabra-reservada? 'SIN)))
  (is (= true (palabra-reservada? 'ATN)))
  (is (= true (palabra-reservada? 'LEN)))
  (is (= true (palabra-reservada? 'MID$)))
  (is (= true (palabra-reservada? 'STR$)))
  (is (= true (palabra-reservada? 'CHR$)))
  (is (= true (palabra-reservada? 'ASC)))
  (is (= true (palabra-reservada? 'GOTO)))
  (is (= true (palabra-reservada? 'ON)))
  (is (= true (palabra-reservada? 'IF)))
  (is (= true (palabra-reservada? 'THEN)))
  (is (= true (palabra-reservada? 'FOR)))
  (is (= true (palabra-reservada? 'TO)))
  (is (= true (palabra-reservada? 'STEP)))
  (is (= true (palabra-reservada? 'NEXT)))
  (is (= true (palabra-reservada? 'GOSUB)))
  (is (= true (palabra-reservada? 'RETURN)))
  (is (= true (palabra-reservada? 'END)))
  (is (= true (palabra-reservada? 'INPUT)))
  (is (= true (palabra-reservada? 'READ)))
  (is (= true (palabra-reservada? 'RESTORE)))
  (is (= true (palabra-reservada? 'PRINT)))
  (is (= false (palabra-reservada? 'JULIAN)))
  (is (= false (palabra-reservada? 'DIEGO)))
  )


(deftest test-operador?
  (is (= true (operador? '+)))
  (is (= true (operador? '-)))
  (is (= true (operador? '/)))
  (is (= true (operador? (symbol "^"))))
  (is (= true (operador? '=)))
  (is (= true (operador? '<>)))
  (is (= true (operador? '<)))
  (is (= true (operador? '<=)))
  (is (= true (operador? '>)))
  (is (= true (operador? '>=)))
  (is (= true (operador? 'AND)))
  (is (= true (operador? 'OR)))
)


(deftest test-dar-error
  (is (= nil (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
  ; El output se tiene que chequear por pantalla
  )


; variable-float?: predicado para determinar si un identificador
; es una variable de punto flotante, por ejemplo:
; user=> (variable-float? 'X)
; true
; user=> (variable-float? 'X%)
; false
; user=> (variable-float? 'X$)
; false
(deftest test-variable-float?
  (is (= true (variable-float? 'X)))
  (is (= false (variable-float? 'X%)))
  (is (= false (variable-float? 'X$)))
)


(run-tests)
