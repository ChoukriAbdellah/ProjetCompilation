;; explication assembleur (+ 1 2) comp = (MOVE 1 R0) concat (push R0) concat (MOVE 2 R0) concat (POP R1) concat (ADD R1 R0)
(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

        ; lit dans un fichier a l'adresse path
(defun readFile (path)
	;; file in : ouvre le fichier dans path, erreur si n'existe pas, l'ouvre en lecture
	(let ( (fin (open path :if-does-not-exist :error :direction :input)) )
		(setf obj '())
		(setf line (read fin nil nil nil))
		;; pour chaque ligne de file in faire
		(loop while (not (equal line nil)) do
			;; ajouter la ligne a obj
			(write line)
			(setq obj (append obj (list line)))
			(setf line (read fin nil nil nil))
		)
		;; fermer file in et retourner obj
		(close fin)
		(return-from readFile obj)
	)
)

(defun type-op(expr) (cond
                        ( (equal expr '+)
                                (list (format nil "(ADD R1 R0)~C" #\linefeed))
                        )
                        ( (equal expr '*)
                                (list (format nil "(MULT R1 R0)~C" #\linefeed ))
                    )
                        ( (equal expr '-)
                                (list (format nil "(SUB R1 R0)~C" #\linefeed))
                        )
                        ( (equal expr '/)
                                (list (format nil "(DIV R1 R0)~C" #\linefeed ))
                        )

                )
   
    )




     
(append (list (format nil "nano" #\linefeed) (list 1 2)))

 (defun expr-arith (expr env)
       
        (cond
        
        ( (atom expr)  (concatString (list (format nil "(MOVE ~a R0)~C" expr  #\linefeed   )) 
        ))
                ( t
                
                  (concatString  (append 
                   (list (lispLineToAsm (second expr) env) ) 
                   (list (format nil "(PUSH R0)~C" #\linefeed))
                   (list (lispLineToAsm (third expr) env) ) 
                  (list  (format nil "(POP R1)~C" #\linefeed))
                  (type-op (car expr)) 
                                 )
                )  
                )                      
        

        
        
        )
)
(defun type-comp(op)
(cond
	( (equal op'>=)
        (list (format nil "(JGE 2)~C" #\linefeed) )
	)
	( (equal op '<=)
	(list (format   nil "(JPE 2)~C" #\linefeed) )
	)
	( (equal op '=) 
        (list (format nil "(JEQ 2)~C" #\linefeed) )	) 
	( (equal op '<) 
        (list (format nil "(JPP 2)~C" #\linefeed) )	) 
	( (equal op '>) 
        (list (format nil "(JPG 2)~C" #\linefeed) )        )
	)
)
  (defun expr-comparateur (expr env)
        
        (cond
        ((not expr) (list  '()) )
        
                ( t
                
                (concatString (append (list (lispLineToAsm (cadr expr) env) )
                (list (format nil "(PUSH R0)~C" #\linefeed))
                (list (lispLineToAsm (caddr expr) env) )
                (list  (format nil "(POP R1)~C" #\linefeed))
                                     
                (list (format nil "(CMP R1 R0)~C" #\linefeed) )
                ; traitement du type de l operateur
                (type-comp (car expr))
                
                ; si le teste est vrai (on ferra les affectations lors de l execution)
                ; car pour le moment on connais les ce que contion le teste
                ;
                ;Si test faux : exemple( du cours R0 = 2 R1 =4 ) 
                ;CMP R0 R1 => 100 (FLT est mis a vrai 1 et les autre a faux 0
                ; car 2 plus petit que 4)
                (list (format nil "(MOVE 0 R0)~C" #\linefeed) )
                (list (format nil "(JMP 1) ~C" #\linefeed) )

		;;Si vrai on récupere et réalise l insctruction cas vrai
                (list (format nil "(MOVE 1 R0)~C" #\linefeed) )
        
        )
        )
        )
        )
  )       

(defun queue-liste (expr &optional env  )
	;; variable local comp stockera les instructions apres compilation
		;; si la variable entrante de cdr est un atome, stocker l'instruction assembleur cdr
		;; sinon retourner liste vide
		(if (atom (cadr expr))
                (list (format nil "(CDR ~a)~C" (cadr expr) #\linefeed) )
		(list '())	
		)

	)
; compiler les car de liste
(defun tete-liste (expr &optionalenv )
	;; variable local comp stockera les instructions apres compilation
	(if (atom (cadr expr))
                (list (format nil "(CAR ~a)~C" (cadr expr) #\linefeed) )
		(list '())	
		)

	)


;;Compile une line vide et la remplace par l'instruction assembleur NOP
(defun compile-skip()
        (list (format nil "(NOP)~C"  #\linefeed) )

)

 (defun operateur? (op)
 
                (cond 
                    ((equal op '+) T)
                    ((equal op '-)  T)
                    ((equal op '*) T)
                    ((equal op '/)  T)
                    ((numberp op)  T )
                    ;else
                    (t nil)

                        )
                                        
                
        )




;; Predicates
; foncton trouvé sur stackoverflow qui permet de compter le nombre d occ de subString dans string
;https://stackoverflow.com/questions/35061185/trouble-counting-subseq-of-a-string-common-lisp
(defun count-substrings (substring string)
  (loop
    with sub-length = (length substring)
    for i from 0 to (- (length string) sub-length)
    when (string= string substring
                  :start1 i :end1 (+ i sub-length))
    count it))

 
                        
; compiler les if
(defun expr-if(instruction env)
 (concatString (append 
                (list (expr-comparateur (cadr instruction) env)) 
                (list (format nil "(CMP 0 R0)~C" #\linefeed))
                
                
                 ; -(-1) pour faire un saut de ligne
                            
                (list  (format nil "(JEQ ~a)~C" 
                        (- (count #\newline  (lispLineToAsm (caddr instruction) env))
                           (count-substrings "LABEL"  (lispLineToAsm (caddr instruction) env) )
                                    -1)  #\linefeed))
                (list (lispLineToAsm (caddr instruction) env) );cas true   
                (list  (format nil "(JMP ~a)~C" (- (count #\newline  (lisplinetoAsm (cadddr instruction) env) )
                                   (count-substrings "LABEL"  (lispLineToAsm (cadddr instruction) env ))
                                    )  #\linefeed)) 
                
                (list (lispLineToAsm (cadddr instruction) env) );cas false
                
               
        
        ) 

        )
)
(defun expr-while(instruction env)
        (concatString (append 
                (list (expr-comparateur (cadr instruction) env)) 
                (list (format nil "(CMP 0 R0)~C" #\linefeed))
                (list  (format nil "(JEQ ~a)~C" (- (count #\newline (write-to-string (caddr instruction)))
                                        (count-substrings "LABEL" (write-to-string (caddr instruction) ))
                                        -1)  #\linefeed))
                (list (lispLineToAsm (caddr instruction) env) );cas true 

                ;Sortie de la boucle si la condition est fasse 
                (list  (format nil "(JMP ~a)~C" ( + (- (count #\newline (write-to-string (caddr instruction)))
                                                        (count-substrings "LABEL" (write-to-string (caddr instruction) ))
                                                        -1) ;; nmobre de saut = nombre d instruction qu'on fait 
                                                        ;; si la condition est vraie + le nombre d instruction
                                                        ;; qu'on réalise dans  le if => jump a(ux) l'insctruction(s)
                                                        ;; false  
                                                        (- (count #\newline (write-to-string (cadr instruction)))
                                                        (count-substrings "LABEL" (write-to-string (cadr instruction) ))
                                                        ))

                                                        #\linefeed))
                        )
        )
)


(defun comparateur?(op)
		(cond 
			((equal op '<)  T)
			((equal op '>)  T)
			((equal op '=)  T)
			((equal op '<=) T)
			((equal op '>=)  T)
                        (t nil)
		)
		
	
)
; remplacer les paramètres par leur registre
(defun replace-params-reg (string_ env ) 
	(let  ( comp) 
		(setf comp string_)
		;;Parcours de la liste des paires dans l'environnement
		(loop for arg in env do 
			;(setf part ) ;;Le motif qui correspond au nom de la variable
			;(setf replacement ) ;;Le motif qui correspond à R suivi du numéro stocké dans l'env.
			(setf comp (replace-all comp
                         (format nil " ~a " (car arg))
                         (format nil " R~a " (cdr arg)) ));;Appel de la fonction pour remplacer <nomVar> par R<numVar>
		)
		(return-from replace-params-reg comp)
        )
)

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos
       )
    )
)

; compiler une ligne

(defun compile-setf (expr env)
	(concatString (append 
		;; compiler la valeur a stocker dans la variable
                       (list ( expr-arith  (third expr) env) )
                        ;; stocker la variable dans l'environement et lui affecter un registre
                        (list (replace-params-reg (format nil "setf fonction(MOVE R0 ~a ) ~%"   (second expr)) env))
                
	              )
        )
)

(defun appel-de-fonction (call env)
	;; variable local comp stockera les instructions apres compilation
	;; variable local nbParam stockera le nombre de parametre de la fonction
	(let (retour nbParam)
		(setf nbParam 0)
		;; pour tous les arguments
                
		(loop for arg in (reverse (cdr call)) do 
			;; compiler les arguments et les empiler dans la pile

			(setf retour (append retour (list (lispLineToAsm  arg env))  
			(list (format nil "(PUSH R0) ~%" ) ) ) ) 
			(setf nbParam (+ nbParam 1)) ; erreur!!
		)
		
                        (setf retour (append retour
                                (list (format nil "(PUSH ~a) ~%" nbParam))
                                (list (format nil "(MOVE FP R1) ~%" ))
                                (list (format nil "(MOVE SP FP) ~%" ))
                                (list (format nil "(MOVE SP R2) ~%" ))
                                (list (format nil "(MOVE ~a R3) ~%" (+ nbParam 1)))
                                (list (format nil "(ADD R3 R2) ~%" ))
                                (list (format nil "(PUSH R2) ~%" ))
                                (list (format nil "(PUSH R1) ~%" ))
                                (list (format nil "(PUSH RA) ~%" ))
                                (list (format nil "(JSR ~a) ~%" (car call)))

                                (list (format nil "(POP R1) ~%" ))
                                (list (format nil "(MOVE R1 RA) ~%" ))

                                (list (format nil "(POP R1) ~%" ))
                                (list (format nil "(MOVE R1 FP) ~%" ))

                                (list (format nil "(POP R1) ~%" ))
                                (list (format nil "(MOVE R1 SP) ~%" ))
                        ) )
			(let (i)
	                        (setf i (lastValueEnv env))
                                (setf retour (append retour (list (format nil "(MOVE FP R3) ~C" #\linefeed))))
		                        (loop for argument in env do
                                                (setf retour (append retour (list (format nil "(INCR R3) ~C" #\linefeed)) 
                                                                            (list (format nil "(LOAD R3 R~a) ~C" i #\linefeed)))
                                                )
                                
                                                (setf i (+ i 1))
		                        )
                (setf retour  (concatString retour) )
	                )
(return-from appel-de-fonction retour)
	)

)
  
 (defun compile-fonction (fct env)
	(let (return) 
		(setf return (list (format nil "(LABEL ~a)~C" (car (cdr fct)) #\linefeed)))
		(let (i)
			(setf i (lastValueEnv env))
			(setf return (append return (list (format nil "(MOVE FP R3) ~C" #\linefeed))))
			(loop for argument in (caddr fct) do
				(setf return (append return (list (format nil "(INCR R3) ~C" #\linefeed)) 
				(list (format nil "(LOAD R3 R~a) ~C" i #\linefeed)))
				)
				(setf env (consPair env argument i))
				(setf i (+ i 1))
			)
		)

		(setf return (concatString (append return (list (replace-params-reg (lispProgToAsm (cdddr fct) env ) env))
		(list (format nil "(RTN) ~C" #\linefeed)))))

		(return-from compile-fonction return)
	)
)


;; fonction facile mais a modidier 
; donner la valeur de la dernière variable d'environnement
(defun lastValueEnv(env)
	(if (null env)
		4
		(cdr (first (last env)))
	)
)

; ajoutee une paire pointée à une liste de paires pointées
(defun consPair (env arg index)
	;;Si l'environnement est null alors
	(if (null env)
		(setf env (list (cons arg index))) ;;Ajoute à la fin une liste de une paire pointée
		(setf env (cons (car env) (consPair (cdr env) arg index) ))  ;;Sinon concatene avec la suite
	)
)


(defun lispLineToAsm (line env)
	
 		;; switch sur les fonctions lisp que l'on peut compiler
 		;; chaque fonction retourne les instructions assembleurs permettant de faire ces operations
 		(cond 
 			((null line)(compile-skip )) ;; skip si la ligne est vide
 			((atom line) (expr-arith line env)) ;; compile expression si la ligne ne contient qu'un atome
 			((equal (car line) 'if) (expr-if line env)) ;; compile if
 			((equal (car line) 'defun)  (compile-fonction line env)) ;; compile fonction (si defun)
 			((equal (car line) 'while)  (expr-while line env)) ;; compile while 
 			((equal (car line) 'setf)  (compile-setf line env)) ;; compile setf
 			((equal (car line) 'car) (tete-liste line env)) ;; compile car
 			((equal (car line) 'cdr) (queue-liste line env)) ;; compile cdr
 			((operateur? (car line)) (expr-arith line env)) ;; compile expression si la ligne effectue une operation arithmetique
 			((comparateur?(car line))(expr-comparateur line env)) ;; compile comp si la ligne effectue une comparaison
 			((atom (car line)) (appel-de-fonction line env)) ;; compile appel de fonction si le premier element est nom (donc un atome)

 		)
 		
 	)

; ecrit str dans un fichier a l'adresse path
(defun writeFile (path str)
	;; file out : ouvre le fichier dans path, cree si n'existe pas, ecrase si existe, l'ouvre en ecriture
	(let ((fout (open path :if-does-not-exist :create :if-exists :supersede :direction :io)))
		;; ecrit str dans file out et ferme file out
		(format fout str)
		(close fout)
	)
)


; compiler un programme (cree un fichier ASM.txt contenant les instructions assembleurs du programme lisp compile)
(defun lispProgToAsm (path env)
	;; variable local codeFinal stockera les instructions apres compilation
	;(writeFile "ASM.txt" ;ecrire dans le fichier asm la concatenation des compilation de
        ; chaque ligaqaqne du programme
        (let (codeFinal ) 
                ;; pour chaque ligne du programme compiler cette ligne
                (loop for line in path do
               
                
                                              
                        (setf codeFinal (concatString (append (list codeFinal)  (list (lispLineToAsm line env)) ) )) 
                )
                (writeFile "ASM.txt" codeFinal)               
                (return-from lispProgToAsm codeFinal)       
                
        )
		;
)

(defun compiler-fichier (path)
        (lispProgToAsm (readfile path) '())
)

(defun compiler-prog (prog)
        (lispProgToAsm prog '())
)

(defun compile-load-prog (vm progr)
	;; compiler
	(compiler-prog progr)
	;; charger le code en memoire
	(charger-fichier vm)
)

(defun compile-load-fichier (vm path)
	;; compiler
	(compiler-fichier path)
	;; charger le code en memoire
	(charger-fichier vm)
)
