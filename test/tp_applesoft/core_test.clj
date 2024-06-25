(ns tp-applesoft.core-test
  (:require [clojure.test :refer :all]
            [tp-applesoft.core :refer :all]))

(deftest test-palabra-reservada?
  (is (= true (palabra-reservada? 'LOAD)))
  (is (= true (palabra-reservada? 'SAVE)))
  (is (= true (palabra-reservada? 'INPUT)))
  (is (= true (palabra-reservada? 'PRINT)))
  (is (= true (palabra-reservada? 'DATA)))
  (is (= true (palabra-reservada? 'READ)))
  (is (= true (palabra-reservada? 'REM)))
  (is (= true (palabra-reservada? 'RESTORE)))
  (is (= true (palabra-reservada? 'CLEAR)))
  (is (= true (palabra-reservada? 'LET)))
  (is (= true (palabra-reservada? 'LIST)))
  (is (= true (palabra-reservada? 'NEW)))
  (is (= true (palabra-reservada? 'RUN)))
  (is (= true (palabra-reservada? 'END)))
  (is (= true (palabra-reservada? 'FOR)))
  (is (= true (palabra-reservada? 'TO)))
  (is (= true (palabra-reservada? 'NEXT)))
  (is (= true (palabra-reservada? 'STEP)))
  (is (= true (palabra-reservada? 'GOSUB)))
  (is (= true (palabra-reservada? 'RETURN)))
  (is (= true (palabra-reservada? 'GOTO)))
  (is (= true (palabra-reservada? 'IF)))
  (is (= true (palabra-reservada? 'ON)))
  (is (= true (palabra-reservada? 'ENV)))
  (is (= true (palabra-reservada? 'EXIT)))
  (is (= true (palabra-reservada? 'ATN)))
  (is (= true (palabra-reservada? 'INT)))
  (is (= true (palabra-reservada? 'SIN)))
  (is (= true (palabra-reservada? 'LEN)))
  (is (= true (palabra-reservada? 'MID$)))
  (is (= true (palabra-reservada? 'ASC)))
  (is (= true (palabra-reservada? 'CHR$)))
  (is (= true (palabra-reservada? 'STR$))))


(deftest test-operador?
  (is (= true (operador? '+)))
  (is (= true (operador? '-)))
  (is (= true (operador? '*)))
  (is (= true (operador? '/)))
  (is (= true (operador? (symbol "^"))))
  (is (= true (operador? '=)))
  (is (= true (operador? '<>)))
  (is (= true (operador? '<)))
  (is (= true (operador? '<=)))
  (is (= true (operador? '>)))
  (is (= true (operador? '>=)))
  (is (= false (operador? (symbol "%")))))


(deftest test-anular-invalidos?
  (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)))))


(deftest test-cargar-linea?
  (is (=
       '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
       (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  (is (=
       '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
       (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  (is (=
       '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
       (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  (is (=
       '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
       (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))))


(deftest expandir-nexts?
  (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))))
  (is (= '((PRINT 1) (NEXT A) (NEXT B) (NEXT C) (PRINT 10) (NEXT D) (NEXT E)) (expandir-nexts '((PRINT 1) (NEXT A , B , C) (PRINT 10) (NEXT D,E)))))
  (is (= '((NEXT)) (expandir-nexts '((NEXT))))))


(deftest test-dar-error?
   (is (= (str "?SYNTAX ERROR") (with-out-str (dar-error 16 [:ejecucion-inmediata 4]))))
   (is (= (str "?ERROR DISK FULL") (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]))))
   (is (= (str "?SYNTAX ERROR IN 100") (with-out-str (dar-error 16 [100 3]))))
   (is (= (str "?ERROR DISK FULL IN 100") (with-out-str (dar-error "?ERROR DISK FULL" [100 3])))))


(deftest test-variable-float?
  (is (= true (variable-float? 'X)))
  (is (= false (variable-float? 'X%)))
  (is (= false (variable-float? 'X$)))
  (is (= false (variable-float? 'MID$))))


(deftest test-variable-integer?
  (is (= true (variable-integer? 'X%)))
  (is (= false (variable-integer? 'X)))
  (is (= false (variable-integer? 'X$))))


(deftest test-variable-string?
  (is (= true (variable-string? 'X$)))
  (is (= false (variable-string? 'X)))
  (is (= false (variable-string? 'X%))))


(deftest contar-sentencias?
  (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]))))


(deftest buscar-lineas-restantes?
  (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
  (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
  (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
  (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
  (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
  (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
  (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
  (is (= (list (list 20 (list 'NEXT 'I) (list 'NEXT 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
  (is (= (list (list 20 (list 'NEXT 'I) (list 'NEXT 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
  (is (= (list (list 20 (list 'NEXT 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
  (is (= (list (list 20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
  (is (= (list (list 20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
  (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}]))))

(deftest continuar-linea?
  (is (=
       (str "?RETURN WITHOUT GOSUB ERROR IN 20")
       (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]))))
  (is (=
       [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]
       (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}]))))


(deftest extraer-data?
  (is (= '() (extraer-data '(()))))
  (is (= '("HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20)))))))


(deftest ejecutar-asignacion?
  (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
  (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
  (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
  (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]))))


(deftest preprocesar-expresion?
  (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
  (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
  (is (= '(1 + 2 / 2 * 0) (preprocesar-expresion '(1 + 2 / 2 * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}]))))


(deftest desambiguar?
  (is (= (list (symbol "-u") 2 '* (symbol "(") (symbol "-u") 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))))
  (is (= (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
  (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
  (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") (symbol "-u") 2 '+ 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")"))))))


(deftest precedencia?
  (is (= 1 (precedencia 'OR)))
  (is (= 2 (precedencia 'AND)))
  (is (= 6 (precedencia '*)))
  (is (= 7 (precedencia '-u)))
  (is (= 9 (precedencia 'MID$))))


(deftest aridad?
  (is (= 0 (aridad 'THEN)))
  (is (= 1 (aridad 'SIN)))
  (is (= 2 (aridad '*)))
  (is (= 2 (aridad 'MID$)))
  (is (= 3 (aridad 'MID3$))))


(deftest eliminar-cero-decimal?
  (is (= 1.5 (eliminar-cero-decimal 1.5)))
  (is (= 1.5 (eliminar-cero-decimal 1.50)))
  (is (= 1 (eliminar-cero-decimal 1.0)))
  (is (= 'A (eliminar-cero-decimal 'A)))
  (is (= 0.5 (eliminar-cero-decimal 0.500)))
  (is (= 0 (eliminar-cero-decimal 0))))


(deftest eliminar-cero-entero?
  (is (= nil (eliminar-cero-entero nil)))
  (is (= "A" (eliminar-cero-entero 'A)))
  (is (= "0" (eliminar-cero-entero 0)))
  (is (= "1.5" (eliminar-cero-entero 1.5)))
  (is (= "1" (eliminar-cero-entero 1)))
  (is (= "-1" (eliminar-cero-entero -1)))
  (is (= "-1.5" (eliminar-cero-entero -1.5)))
  (is (= ".5" (eliminar-cero-entero 0.5)))
  (is (= "-.5" (eliminar-cero-entero -0.5)))
  (is (= 0 (eliminar-cero-entero false)))
  (is (= 1 (eliminar-cero-entero true))))