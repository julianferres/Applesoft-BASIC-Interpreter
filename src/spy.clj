 ; https://medium.com/appsflyer/repl-based-debugging-in-clojure-278fb468a33

; para facilitar los seguimientos
; spy se puede usar asi (spy) en macros ->>
; y tambien para encerrar expresiones cuyo valor, ademas de ser retornado al evaluar la expresion, se imprime

(defn spy [x] (prn x) x)


; string-a-tokens: Recibe una cadena y devuelve la RI (representacion intermedia) que el ejecutor del interprete procesara
 
(defn string-a-tokens [s]
  (let [nueva (str s ":"),
        mayu (clojure.string/upper-case nueva),
        sin-cad (clojure.string/replace mayu #"\"(.*?)\"" #(clojure.string/join (take (+ (count (% 1)) 2) (repeat "@")))),
        ini-rem (clojure.string/index-of sin-cad "REM"),
        pre-rem (subs mayu 0 (if (nil? ini-rem) (count mayu) ini-rem)),
        pos-rem (subs mayu (if (nil? ini-rem) (- (count mayu) 1) (+ ini-rem 3)) (- (count mayu) 1)),
        sin-rem (->> pre-rem
(spy)
                    (re-seq #"EXIT|ENV|DATA[^\:]*?\:|REM|NEW|CLEAR|LIST|RUN|LOAD|SAVE|LET|AND|OR|INT|SIN|ATN|LEN|MID\$|STR\$|CHR\$|ASC|GOTO|ON|IF|THEN|FOR|TO|STEP|NEXT|GOSUB|RETURN|END|INPUT|READ|RESTORE|PRINT|\<\=|\>\=|\<\>|\<|\>|\=|\(|\)|\?|\;|\:|\,|\+|\-|\*|\/|\^|\"[^\"]*\"|\d+\.\d+E[+-]?\d+|\d+\.E[+-]?\d+|\.\d+E[+-]?\d+|\d+E[+-]?\d+|\d+\.\d+|\d+\.|\.\d+|\.|\d+|[A-Z][A-Z0-9]*[\%\$]?|[A-Z]|\!|\"|\#|\$|\%|\&|\'|\@|\[|\\|\]|\_|\{|\||\}|\~")
(spy)
                    (map #(if (and (> (count %) 4) (= "DATA" (subs % 0 4))) (clojure.string/split % #":") [%]))
(spy)
                    (map first)
(spy)
                    (remove nil?)
(spy)
                    (replace '{"?" "PRINT"})
(spy)
                    (map #(if (and (> (count %) 1) (clojure.string/starts-with? % ".")) (str 0 %) %))
(spy)
                    (map #(if (and (>= (count %) 4) (= "DATA" (subs % 0 4))) (let [provisorio (interpose "," (clojure.string/split (clojure.string/triml (subs % 4)) #",[ ]*"))] (list "DATA" (if (= ((frequencies %) \,) ((frequencies provisorio) ",")) provisorio (list provisorio ",")) ":")) %))
(spy)
                    (flatten)
(spy)
                    (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux)) aux (symbol %))))
(spy)
                    (#(let [aux (first %)] (if (and (integer? aux) (not (neg? aux))) (concat (list aux) (list (symbol ":")) (rest %)) %)))
(spy)
                    (partition-by #(= % (symbol ":")))
(spy)
                    (remove #(.contains % (symbol ":")))
(spy)
                    (#(if (and (= (count (first %)) 1) (number? (ffirst %))) (concat (first %) (rest %)) %))
(spy)
					)]
(spy
       (if (empty? pos-rem)
           sin-rem
           (concat sin-rem (list (list 'REM (symbol (clojure.string/trim pos-rem))))))
) ; fin de spy		   

) ; fin de let

) ; fin de defn




; Ejemplo para el REPL. Ingresar: 12?.5:data1,dos,3,hola mundo:print"hi data:";:data 4,5:rem data6,7
;
; user => (do (string-a-tokens (read-line)) true)
; 12?.5:data1,dos,3,hola mundo:print"hi data:";:data 4,5:rem data6,7
; "12?.5:DATA1,DOS,3,HOLA MUNDO:PRINT\"HI DATA:\";:DATA 4,5:"
; ("12" "?" ".5" ":" "DATA1,DOS,3,HOLA MUNDO:" "PRINT" "\"HI DATA:\"" ";" ":" "DATA 4,5:")
; (["12"] ["?"] [".5"] [":"] ["DATA1,DOS,3,HOLA MUNDO"] ["PRINT"] ["\"HI DATA:\""] [";"] [":"] ["DATA 4,5"])
; ("12" "?" ".5" ":" "DATA1,DOS,3,HOLA MUNDO" "PRINT" "\"HI DATA:\"" ";" ":" "DATA 4,5")
; ("12" "?" ".5" ":" "DATA1,DOS,3,HOLA MUNDO" "PRINT" "\"HI DATA:\"" ";" ":" "DATA 4,5")
; ("12" "PRINT" ".5" ":" "DATA1,DOS,3,HOLA MUNDO" "PRINT" "\"HI DATA:\"" ";" ":" "DATA 4,5")
; ("12" "PRINT" "0.5" ":" "DATA1,DOS,3,HOLA MUNDO" "PRINT" "\"HI DATA:\"" ";" ":" "DATA 4,5")
; ("12" "PRINT" "0.5" ":" ("DATA" ("1" "," "DOS" "," "3" "," "HOLA MUNDO") ":") "PRINT" "\"HI DATA:\"" ";" ":" ("DATA" ("4" "," "5") ":"))
; ("12" "PRINT" "0.5" ":" "DATA" "1" "," "DOS" "," "3" "," "HOLA MUNDO" ":" "PRINT" "\"HI DATA:\"" ";" ":" "DATA" "4" "," "5" ":")
; (12 PRINT 0.5 : DATA 1 , DOS , 3 , HOLA MUNDO : PRINT "HI DATA:" ; : DATA 4 , 5 :)
; (12 : PRINT 0.5 : DATA 1 , DOS , 3 , HOLA MUNDO : PRINT "HI DATA:" ; : DATA 4 , 5 :)
; ((12) (:) (PRINT 0.5) (:) (DATA 1 , DOS , 3 , HOLA MUNDO) (:) (PRINT "HI DATA:" ;) (:) (DATA 4 , 5) (:))
; ((12) (PRINT 0.5) (DATA 1 , DOS , 3 , HOLA MUNDO) (PRINT "HI DATA:" ;) (DATA 4 , 5))
; (12 (PRINT 0.5) (DATA 1 , DOS , 3 , HOLA MUNDO) (PRINT "HI DATA:" ;) (DATA 4 , 5))
; (12 (PRINT 0.5) (DATA 1 , DOS , 3 , HOLA MUNDO) (PRINT "HI DATA:" ;) (DATA 4 , 5) (REM DATA6,7))

; Observacion:
; probablemente haya quedado de otra iteracion y ahora este sobrando el (remove nil?) en ->>


