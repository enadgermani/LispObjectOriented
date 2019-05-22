;;;;Implementazione del OO in Lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;PRIMITIVE;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Funzione usata per definire una nuova classe
(defun def-class (class-name parents &rest slot-values)
  (if (input-validation class-name parents slot-values)  
      (if (not (get-class-spec class-name)) ;;;; Cerca se la classe sia stata giÃ  definita
          (if (has-alive-parents parents)
              (if (null parents) ;;;; Cerca se orfana. La funzione precedente ha null args come caso base
                  (add-class-spec class-name (append (list parents) (values-assignament slot-values)))
													;;;; Aggiunge nome + specifiche all'hash table
                (if (listp parents)
                    (add-class-spec class-name 
                                    (remove Nil 
                                            (append (list parents) 
                                                    (confirm (inheritance (list parents) (values-assignament slot-values))))
                                    ));;;Aggiunge nome e specifiche all'hash table
                  (error "errore nell'assegnamento slot-values")))
            (error "Classe padre non  definita"))
        (error "classe gia' definita"))
        (error "Input errato")))
	

	
;;;; Instanzia una nuova classe precedentemente definita
(defun new (class-name &rest slot-values)
  (if (symbolp class-name)	 			;;;;controllo minimo 
      (if (= 0 (mod  (length slot-values) 2))		;;;;numero pari/0 di elementi a slot-values		
		(if (get-class-spec class-name)  	;;;;Controlla l'esistenza della classe
              (if (validate-slot class-name slot-values);;;;L'istanza ha solo e soltanto i metodi definiti dalla struttura di classe
                  (if (validate-slot-values slot-values)   	;;;;Controllo aggiuntivo sulla struttura di slot-values.
                      (append (list class-name) ;;;; Creo una lista con il nome e gli slot 
                              (values-assignament slot-values)
                              (rest (get-class-spec class-name)))
                    (error "Slot-value problematico"))
                (error "validate-slot fallito"))
            (error "Classe non definita"))
        (error "Slot-value dispari"))
    (error "Nome della classe errato "))) 

	
	
;;;;Controlli preliminari, chiamata e controllo dell'output.
(defun getv (instance slot-name) 
  (if  (and (not (null instance))  (symbolp slot-name))
      (if (not (null (get-slot (rest instance) slot-name)))
          (rest (get-slot (rest instance) slot-name)))))

   
  

  
;;;;Crea la lista di slot-names da trovare. utilizza getvx-r
(defun getvx (instance &rest slot-names)
  (if (= (length slot-names) 1) ;;;Caso banale, se true allora getvx=getv.
      (if (listp (first slot-names)) 	;;;;controllo input
          (getv instance (first(first slot-names))) ;;;;Output banale
        (getv instance (first slot-names)))			;;;;Output banale
    (getvx-r (getv instance (first slot-names))  (rest slot-names)))) ;;;Utilizzo concreto
	
	
	
	
	
  

;;;;Primo controllo di def-class. 
(defun input-validation (name parents slot-values) 
  (if (and (symbolp name)
           (list parents)
           (or (typep slot-values 'atom)
               (= 0 (mod (length slot-values) 2)))) 
      (validate-slot-values slot-values)
    Nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;GESTIONE DEI METODI;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;  Crea la funzione
(defun rewrite-method-code (method-spec)
   (list 'lambda (append (list 'this) (second method-spec));;;;Creo la funzione lambda come lista il cui primo elemento e' quotato
				 (cons 'progn (rest (rest method-spec))))) ;;;;progn e' il miracolo.
											;;;;Costringo lisp a valutare tutto cio' che segue

;;;;(lambda 'this () progn ((princ "a") (princ "b"))

(defun process-method (method-name method-spec)
  (setf (fdefinition method-name) 
	(lambda (this &rest args)   
	  (apply (getv this  method-name)
		 (append (list this)  args))))	
  (eval (rewrite-method-code  method-spec))) 
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;FUNZIONI ACCESSORIE;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
;;;;Funzione principale di def-class e New. +
(defun values-assignament (slot-values)
  (if (null slot-values)
      () ;;;; caso base
      (if (and (not (atom (second slot-values))) ;;;; gestione metodi
	       (equal (car (second slot-values)) 
		     '=>))
	  (append (list (first slot-values)) ;;;; Sostituisce con il metodo
	  (list (process-method 
					(first slot-values) ;;;Method-name
					(second slot-values)));;;; Method-spec
		  (values-assignament (rest (rest slot-values))))

	  (append (if (= (count (first slot-values) (rest slot-values)) 0)
                         (append (list (first slot-values) ;;;; aggiunge slot-name e slot-value
                                       (second slot-values))
                                ) ())
                    (values-assignament (rest (rest slot-values))))))) 



;;;;prima e principale funzione di gestione dell'ereditarieta'
;;;;Si occupa dei parents non 
(defun inheritance (parents slot-values)
  (if (null parents) () ;
    (append  ( add-spec-to-new-class (rest (get-class-spec (first parents))) slot-values) ;;;;applica a specifiche d classe e slot values originali
             (inheritance (rest parents) slot-values) ;;;;applica a resto dei parents
             )))
                  
				  
;;;; funzione che elimina eventuali doppioni.				  
(defun confirm (slots) 
  (if (null slots) ()
    (if (> (count (first slots) (cdr slots)) 0)
        (append (confirm (rest (rest slots))))
      (append (list(first slots))
              (list(second slots))
              (confirm (rest (rest slots)))))))

;;;;Prende in input due liste e riporta l'unione senza duplicati
;;;;Ovviamente in base coppie di nome - valore
;;;;Si selezionano gli elementi in base al nome della variabile).
;;;;La precedenza va agli argomenti dell'istanza.
(defun add-spec-to-new-class (class-values slot-values) 
 (if (null class-values) (append slot-values)
   (if (not (member (first  class-values) slot-values));; Precedenza agli slot-values. In alternativa class-values.
        (append  (add-spec-to-new-class (rest (rest class-values))  (append (list (first class-values))
                                                             (list (second class-values))
                                                             slot-values)))
       (append (add-spec-to-new-class (rest (rest class-values)) slot-values) )
                                                     
          ) 
  ))



	  
;;;;Fallisce se uno dei parents non e' stato definito
(defun has-alive-parents (parents) 
  (if (null parents) T
    (if (get-class-spec (first parents))
        (has-alive-parents (rest parents))
      Nil)))

	  
;;;;Controlla la lista degli slot-values  
(defun validate-slot-values (slot-values)
  (if (null slot-values) 
      T						;;caso base
    (if (symbolp (first slot-values)) ;;;; Il primo elemento deve essere un simbolo
        (validate-slot-values (rest (rest slot-values))) ;;;; Controllo il terzo elemento
      Nil)))

;;;;Ogni nome variabile deve esistere nelle specifiche di classe
(defun validate-slot (class-name  slot-values)
  (if (not (null slot-values))
    (if (member (first slot-values)  (get-class-spec class-name))
        (validate-slot class-name (rest (rest slot-values))) 
      )
    T)
  )

  
;;;;Il vero getv: Ricorsivamente trova un valore in una lista e ritorna il successivo
(defun get-slot (list-arg slot-name)
  (if (not (null list-arg)) ;;;;caso base
		(if (equal (first list-arg) slot-name) ;;;; Slot-name trovato
			    (cons slot-name (second list-arg))  ;;;; Ritorna (slot-name, slot-value)
            (get-slot (rest (rest list-arg)) slot-name)) ;;;; Passo ricorsivo
    Nil))	
	

	
	
	
;;;;Valuta gli slot-values senza formare una nuova lista ogni chiamata
(defun getvx-r (instance slot-names) 
  (if (= (length slot-names) 1)  ;;;;Caso base.
        (getv instance (first slot-names))
      (getvx-r (getv instance (first slot-names)) (rest slot-names))))
	  
	  
 
;;;; Creazione hash table per salvare le classi
(defparameter *classes-specs* (make-hash-table))

;;;; Def-class serve a definire gli argomenti di questa funzione.
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) ;;;; Collega il nome della classe con le specifiche
	class-spec) name)
 ;;;(car (list name))) ;;;; Ritorna il nome della classe  PROVO A COMMENTARE STA COSA

;;;; 
;;;; Recupera dati dall'hash code. Funziona come definito corregge errori di input (lista di un elemento)
(defun get-class-spec (name)
  (if  (not (listp name))
      (gethash name *classes-specs*)
    (gethash (first name) *classes-specs*)))
	
