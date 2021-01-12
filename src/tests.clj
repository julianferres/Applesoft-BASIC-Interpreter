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
  (is (= false (variable-float? "X$")))
)

(deftest test-variable-integer?
  (is (= false (variable-integer? 'X)))
  (is (= true (variable-integer? 'X%)))
  (is (= false (variable-integer? 'X$)))
  (is (= false (variable-integer? "X$")))
  )

(deftest test-variable-string?
  (is (= false (variable-string? 'X)))
  (is (= false (variable-string? 'X%)))
  (is (= true (variable-string? 'X$)))
  (is (= false (variable-string? "X$")))
  )

(deftest test-eliminar-cero-decimal
  (is (= '1.5 (eliminar-cero-decimal 1.5)))
  (is (= '1.5 (eliminar-cero-decimal 1.50)))
  (is (= '-1.5 (eliminar-cero-decimal -1.5000000)))
  (is (= '1 (eliminar-cero-decimal 1.0)))
  (is (= '-1 (eliminar-cero-decimal -1.0)))
  (is (= 'A (eliminar-cero-decimal 'A)))
  )

(deftest test-eliminar-creo-entero
  (is (nil? (eliminar-cero-entero nil)))
  (is (= "A" (eliminar-cero-entero 'A)))
  (is (= "0" (eliminar-cero-entero 0)))
  (is (= "1.5" (eliminar-cero-entero 1.5)))
  (is (= "1" (eliminar-cero-entero 1)))
  (is (= "-1" (eliminar-cero-entero -1)))
  (is (= "-1.5" (eliminar-cero-entero -1.5)))
  (is (= ".5" (eliminar-cero-entero 0.5)))
  (is (= "-.5" (eliminar-cero-entero -0.5)))
  )

(deftest test-expandir-nexts
  (let [n (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))
        n2 (list '(PRINT 1) '(PRINT 3))
        n3 (list '(NEXT A) (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C))
        ]
    (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts n)))
    (is (= '((PRINT 1) (PRINT 3) (expandir-nexts n2))))
    (is (= '((NEXT A) (NEXT A) (NEXT B) (NEXT C)) (expandir-nexts n3)))
  )
)

(deftest test-contar-sentencias
  (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
)

(deftest test-buscar-lineas-restantes
  (is (nil? (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  (is (nil? (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
  (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
  (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= (list '(10) '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
  (is (= (list '(15 (X = X + 1)) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
  (is (= (list '(15) (list '20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
  (is (= '((20 (NEXT I) (NEXT J)))  (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
  (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
  (is (= '((20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
  (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
  (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
  (is (nil? (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))


  )

; sentencias DATA, por ejemplo:
; user=> (extraer-data '(()))
; ()
; user=> (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
; ("HOLA" "MUNDO" 10 20)
(deftest test-extraer-data
  (is (= '() (extraer-data '(()))))
  (is (= '(HOLA MUNDO 10 20) (extraer-data '((10 (PRINT X) (REM ESTE NO) (DATA 30)) (20 (DATA HOLA)) (100 (DATA MUNDO 10 20)))) ))
  )
(run-tests)
