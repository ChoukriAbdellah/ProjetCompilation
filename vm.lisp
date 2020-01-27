;; explication assembleur (+ 1 2) comp = (MOVE 1 R0) concat (push R0) concat (MOVE 2 R0) concat (POP R1) concat (ADD R1 R0)
(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun type-op(expr) (cond
                        ( (equal expr '+)
                                (list (format nil "(ADD R1 R0)"))
                        )
                        ( (equal expr '*)
                                (list "MULT R1 R0")
                        )
                        ( (equal expr '-)
                                (list "SUB R1 R0")
                        )
                        ( (equal expr '/)
                                (list "DIV R1 R0")
                        )

                )
   
    )



        (defun writeFile (path str)
	;; file out : ouvre le fichier dans path, cree si n'existe pas, ecrase si existe, l'ouvre en ecriture
	(let ((fout (open path :if-does-not-exist :create :if-exists :supersede :direction :io)))
		;; ecrit str dans file out et ferme file out
		(format fout str)
		(close fout)
	)
)

     
(append (list (format nil "nano" #\linefeed) (list 1 2)))

 (defun expr-arith (expr env)
        
        (cond
        ((not expr) (list  '()) )
        ( (atom expr) (list (format nil "(MOVE ~a R0)~C" expr  #\linefeed   )) 
        )
                ( t
                
                   (concatString (append (expr-arith (second expr) env)
                (list (format nil "(PUSH R0)~C" #\linefeed))
                (expr-arith (third expr) env)
                (list  (format nil "(POP R1)~C" #\linefeed))
                (type-op (car expr)) )  )                      
        

        
        )
        )
        )
(defun type-comp(op)
(cond
	( (equal op'>=)
        (list (format nil "(JGE 2)~C" #\linefeed) )
	)
	( (equal op '<=)
	(list (format   nil "(JLE 2)~C" #\linefeed) )
	)
	( (equal op '=) 
        (list (format nil "(JEQ 2)~C" #\linefeed) )	) 
	( (equal op '>) 
        (list (format nil "(JGT 2)~C" #\linefeed) )	) 
	( (equal op '>) 
        (list (format nil "(JLT 2)~C" #\linefeed) )        )
	)
)
  (defun expr-comparateur (expr env)
        
        (cond
        ((not expr) (list  '()) )
        
                ( t
                
                (concatString (append (expr-arith (cadr expr) env)
                (list (format nil "(PUSH R0)~C" #\linefeed))
                (expr-arith (caddr expr) env)
                (list  (format nil "(POP R1)~C" #\linefeed))
                                     
                (list (format nil "(CMP R0 R1)~C" #\linefeed) )
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

(defun queue-liste (expr env)
	;; variable local comp stockera les instructions apres compilation
		;; si la variable entrante de cdr est un atome, stocker l'instruction assembleur cdr
		;; sinon retourner liste vide
		(if (atom (cadr expr))
                (list (format nil "(CDR ~a)~C" (cadr expr) #\linefeed) )
		(list '())	
		)

	)

;;Compile une line vide et la remplace par l'instruction assembleur NOP
(defun compile-skip(line env)
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
                
                
                (list  (format nil "(JEQ ~a)~C" (- (count #\newline (write-to-string (caddr instruction)))
                                 (count-substrings "LABEL" (write-to-string (caddr instruction) ))
                                    -1)  #\linefeed)) ; -(-1) pour faire un saut de ligne
                (expr-arith (caddr instruction) env);cas true               
                
                (list  (format nil "(JMP ~a)~C" (- (count #\newline (write-to-string (cadddr instruction)))
                                   (count-substrings "LABEL" (write-to-string (cadddr instruction)))
                                    )  #\linefeed))
                (expr-arith (cadddr instruction) env) ;cas false
                
               
        
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
                (expr-arith (caddr instruction) env);cas true 

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
	
		(concatString (append 
                
		;;Parcours de la liste des paires dans l'environnement
		(loop for arg in env do 
			 ;;Le motif qui correspond au nom de la variable
			(list  (format nil " R~a " (cdr arg))) ;;Le motif qui correspond à R suivi du numéro stocké dans l'env.
			(list  (replace-all (list string_) 
                        (list  (format nil " ~a " (car arg)))
                        (list  (format nil " R~a " (cdr arg))) ));;Appel de la fonction pour remplacer <nomVar> par R<numVar>
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
; compiler une ligne

(defun compile-setf (expr env)
	(concatString (append 
		;; compiler la valeur a stocker dans la variable
                        ( expr-arith  (third expr) env)
                        ;; stocker la variable dans l'environement et lui affecter un registre
                        (list (replace-params-reg (format nil "(MOVE R0 ~a ) ~%"   (second expr)) env))
                
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

			(setf retour (append retour (list (compile-line  arg env))  
			(list (format nil "(PUSH R0) ~%" ) ) ) ) 
			(setf nbParam (+ nbParam 1)) ; erreur!!
		)
		;; ordre d'appel :
		;; 		empiler le nombre de parametre
		;; 		stocker l'ancien FP
		;; 		affecter le nouveau FP (egal SP actuel)
		;; 		calculer et empile l'ancien SP (SP actuel plus le nombre de parametre + 1 [pour l'empilement du nombre de parametres])
		;; 		empiler l'ancien FP
		;; 		empiler l'ancien RA
		;; 			sauter a la fonction appelee (stocker prochaine instruction dans RA)
		;; 			fonction finie (qui a donc utilisee le return [RTN])
		;; 		depiler et revenir a l'ancien RA 
		;; 		depiler et revenir a l'ancien FP
		;; 		depiler et revenir a l'ancien SP (revient a depiler tous les parametres)
		;; 		recuperer l'ancien environemment 
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
                (setf retour (concatString retour))
	                )
(return-from appel-de-fonction retour)
	)
        
)
