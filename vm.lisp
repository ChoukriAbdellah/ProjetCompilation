;explication instruction.lisp
 ;la vm :
 ;la vm est une sorte de class avec des attributs 
 ;-name   -> nom de la vm
 ;-       -> taille de la mémoire (représenté sous forme d'un tableau) comme expliqué dans le cours 
;          La mémoire est un ensemble fini de N cellules (ou mots)

;make vm consiste donc a:
 ;   -associer à chaque registre de base 
 ;   son numéro d'instruction qui correspond à sa valeur 

 ;   -initialiser les valeurs des flags à 0
    
 ;   - et son état  1 = Off et 0 = on 

(require "compilation.lisp")
; definit un symbole dans une variable;
(defun set-Symb (vm nom val)
;; voir ça  comme une class (get class attribut) valeur a affecter 
	(setf (get vm nom) val) 
)
; initialisation memoire
(defun init-mem (nom &optional(taille 1000)) (set-Symb nom 'mem (make-array taille)) )
; initialisation des flags
(defun set-flag-init (vm)
	(set-Symb vm 'DEQ 0)
	(set-Symb vm 'DPG 0)
	(set-Symb vm 'DPP 0)

)

; creation de la machine virtuelle (taille de base 1000)
(defun make-vm (nom &optional (taille 1000))
	(init-mem nom taille) ; memoire

	;; Registre de base
	(set-Symb nom 'labels (make-hash-table)) ; label (cle = nom, value = numero instruction)
	(set-Symb nom 'SP taille) ; stack pointer 	sommet de la pile
	(set-Symb nom 'MP (- taille (* taille 0.10))) ; max pile
	(set-Symb nom 'FP taille) ; frame pointer   qui permet de structurer( et donc faciliter )la pile à l'aide des blocs
	(set-Symb nom 'RA 0) ; return adress
	(set-Symb nom 'PC 0) ; program counter
	
	;; Flag
	(set-flag-init nom)

	;; Register
	; (set-Symb nom 'R0 0)
	; (set-Symb nom 'R1 0)
	; (set-Symb nom 'R2 0)
	; (set-Symb nom 'R3 0)

	;; VM State (0 on, 1 off)
	(set-Symb nom 'state 0)
)

(make-vm 'demo)

(defun ràz-vm ()
	(make-vm 'demo)
)

(defun lancer-vm ()
	(exec-vm 'demo)
)

(defun get-resultat-vm ()
	(get 'demo 'R0)
)

(defun get-contenu-vm ()
	(get 'demo 'mem)
)

; en fait ici il prend pas une liste d'instruction mais la liste
; contenant tout les instruction genre ( (RTN truc ) (....))
(defun function-length (instr &optional (size 0) )
	(if (equal (caar instr) 'RTN) ; donc ici il regarde la tete de l'instruction
								  ; ( RTN .. )
		(+ size 1)
		(function-length (cdr instr) (+ size 1))
	)
)
; donc en fin de compte cette fonction permet de calculer le nombre 
; d'instruction 


; charge une liste d'instruction dans la memoire
(defun loader (vm instr &optional(ptr (get vm 'PC)))
	(cond
		((not (equal (car instr) NIL))
			(cond
				(
					;; si instruction est label -> stocke le numero de l'instruction dans la hash table labels
					(equal(caar instr) 'LABEL)
						(setf (gethash (cadar instr) (get vm 'labels)) ptr)
						(set-Symb vm 'PC (+ (get vm 'PC) (function-length (cdr instr))))
						(loader vm (cdr instr) ptr)
				)
					;; sinon charger instruction a la suite dans la memoire
				(
					(setf (aref (get vm 'mem) ptr) (car instr))
					(loader vm (cdr instr) (+ ptr 1))
				)
			)
		)
	)
)

; compiler et charger le code assembleur dans la memoire
(defun charger-fichier (vm)
	;; charger le code en memoire
	(loader vm (readFile "ASM.txt"))
)

; compiler et charger le code assembleur dans la memoire
(defun charger-asm (vm asm)
	;; charger le code en memoire
	(loader vm asm)
)

; On prend la liste des intructions et pour chaque instruction on la charge
; on distingue 2 cas d'instruction :
; si la tete de l'intruction est un label sa veut dire qu'on défini une f(x)
; ex label racineCarre 
;ou une procedure  dans ce cas on stock le numéro de l'instruction dans la 
; hashTable conçu pour stoker les labels qu'on a crée dans make-vm 
; Le compteur ordinale,  va donc pointer sur l'intruction suivante cad l'intruction
; qui suit la définition de fonction

;Sinon c'est une simple instruction alors on la charge
; donne un nom ( un symbol ) et une valeur 
;l'instruction suivante sera à PC +1 
;=> Cette fonction prend chaque instruction de la liste contenat toute les
; instructions et charge dans la vm

; defini flag equal
(defun set-flag-DEQ (vm)
	(set-flag-init vm)
	(set-Symb vm 'DEQ 1)
)

; defini flag plus grand
(defun set-flag-DPG (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPG 1)
)

;defini flag plus petit
(defun set-flag-DPP (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPP 1)
)

; execute le code stocker dans la memoire de vm
(defun exec-vm (vm)
	(loop while (not (or (equal (aref (get vm 'mem) (get vm 'PC)) NIL) (equal (get vm 'state) 1)))
		do
				#| (write (format nil " trace : ~a ~%" (aref (get vm 'mem) (get vm 'PC)))) |#
		(cond 
			;; switch sur les instructions charger (appelle les fonctions lisp correspondantes dans instruction.lisp)
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'MOVE)	
				(vm-move	vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'LOAD)	
				(vm-load	vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'STORE)	
				(vm-store	vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'INCR)	
				(vm-incr	vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'DECR)	
				(vm-decr	vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'ADD)	
				(vm-add		vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'SUB)	
				(vm-sub		vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'MULT)	
				(vm-mult	vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'DIV)	
				(vm-div		vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'PUSH)	
				(vm-push	vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'POP)	
				(vm-pop		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JPG)	
				(vm-jpg		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JEQ)	
				(vm-jeq		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JPP)	
				(vm-jpp		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JGE)	
				(vm-jge		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JPE)	
				(vm-jpe		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JMP)	
				(vm-jmp		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'JSR)	
				(vm-jsr		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'RTN)	
				(vm-rtn		vm))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'CMP)	
				(vm-cmp		vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'HALT)	
				(vm-halt	vm))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'NOP)	
				(vm-nop		vm))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'CAR)	
				(vm-car		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'CDR)	
				(vm-cdr		vm	(cadr (aref (get vm 'mem) (get vm 'PC)))))
			( (equal (car (aref (get vm 'mem) (get vm 'PC))) 'CONS)	
				(vm-cons	vm	(cadr (aref (get vm 'mem) (get vm 'PC))) (caddr (aref (get vm 'mem) (get vm 'PC)))))
		)
		;; incrementer pc (passer a l'instruction suivante)
		(incf (get vm 'PC))
	)
	"Fin de VM"
)

;; INSTRUCTIONS MACHINE VIRTUELE
;; _____________________________________________________________________

; MOVE P1 P2
(defun vm-move (vm P1 P2)
	(if (symbolp P1)
		(setf (get vm P2) (get vm P1))
		(setf (get vm P2) P1)
	)
)

; LOAD adr P
(defun vm-load (vm adr P)
	(if (symbolp adr)
		(setf (get vm P) (aref (get vm 'mem) (get vm adr)))
		(setf (get vm P) (aref (get vm 'mem) adr))

	)
)

; STORE P adr
(defun vm-store (vm P adr)
	(if (symbolp P)
		(setf (aref (get vm 'mem) adr) (get vm P))
		(setf (aref (get vm 'mem) adr) P)
	)
)

; INCR P
(defun vm-incr (vm P)
	(incf (get vm P))
)

; DECR P
(defun vm-decr (vm P)
	(decf (get vm P))
)

; ADD P1 P2
(defun vm-add (vm P1 P2)
	(setf (get vm P2) (+ (get vm P1) (get vm P2)))
)

; SUB P1 P2
(defun vm-sub (vm P1 P2)
	(setf (get vm P2) (- (get vm P1) (get vm P2)))
)

; MULT P1 P2
(defun vm-mult (vm P1 P2)
	(setf (get vm P2) (* (get vm P1) (get vm P2)))
)

; DIV P1 P2
(defun vm-div (vm P1 P2)
	(if (not (equal (get vm P1) 0))
		(setf (get vm P2) (/ (get vm P1) (get vm P2)))
	)
)

; PUSH P
(defun vm-push (vm P)
    (vm-decr vm 'SP)
    (cond
        (
            (< (get vm 'SP) (get vm 'MP))
                (setf (get vm 'state) 1)
                (write "ERREUR : la pile a deborde")
        )
        (
            (vm-store vm P (get vm 'SP))
        )
    )
)

; POP P
(defun vm-pop (vm P)
	(vm-load vm (get vm 'SP) P)
	(vm-incr vm 'SP)
)

; JPG etiq
(defun vm-jpg (vm etiq)
	(if (equal (get vm 'DPG) 1)
		(vm-jmp vm etiq)
	)
)

; JEQ etiq
(defun vm-jeq (vm etiq)
	(if (equal (get vm 'DEQ) 1)
		(vm-jmp vm etiq)
	)
)

; JPP etiq
(defun vm-jpp (vm etiq)
	(if (equal (get vm 'DPP) 1)
		(vm-jmp vm etiq)
	)
)

; JGE etiq
(defun vm-jge (vm etiq)
	(if (or (equal (get vm 'DPG) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp vm etiq)
	)
)

; JPE etiq
(defun vm-jpe (vm etiq)
	(if (or (equal (get vm 'DPP) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp vm etiq)
	)
)

; JMP etiq
(defun vm-jmp (vm etiq)
	(if (integerp etiq)
		(setf (get vm 'PC) (+ (get vm 'PC) etiq))
		(setf (get vm 'PC) (- (gethash etiq (get vm 'labels)) 1))
	)
)

; JSR etiq
(defun vm-jsr (vm etiq)
	(setf (get vm 'RA) (get vm 'PC))
	(vm-jmp vm etiq)
)

; RTN
(defun vm-rtn (vm)
	(setf (get vm 'PC) (get vm 'RA))
)

; CMP atom P2
(defun vm-cmp-atom (vm P1 P2)
	(cond
		((equal P1 (get vm P2)) (set-flag-DEQ vm))
		((< P1 (get vm P2)) (set-flag-DPP vm))
		((> P1 (get vm P2)) (set-flag-DPG vm))
	)
) 

; CMP P1 P2
(defun vm-cmp-reg (vm P1 P2)
	(cond
		((equal (get vm P1) (get vm P2)) (set-flag-DEQ vm))
		((< (get vm P1) (get vm P2)) (set-flag-DPP vm))
		((> (get vm P1) (get vm P2)) (set-flag-DPG vm))
	)
)

; CMP P1 P2
(defun vm-cmp (vm P1 P2)
	(if (symbolp P1)
		(vm-cmp-reg vm P1 P2)
		(vm-cmp-atom vm P1 P2)
	)
)

; HALT 
(defun vm-halt (vm)
	(set-Symb vm 'state 1)
)

; NOP
(defun vm-nop (vm))

; CAR P
(defun vm-car (vm P)
	(setf (get vm P) (car (get vm P)))
)

; CDR P
(defun vm-cdr (vm P)
	(setf (get vm P) (cdr (get vm P)))
)


; CONS P1 P2
(defun vm-cons (vm P1 P2)
	(setf (get vm P1) (cons (get vm P1) (get vm P2)))
)
