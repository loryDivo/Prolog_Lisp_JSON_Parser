#||
	 Membri del gruppo (cognome, nome	matricola):
	 Belotti, Manuel		    795599
	 Di Vito, Lorenzo		    793128
	 Lobba, Diego			    795702
||#

;;;; Per un introduzione generale al parsing, vedere il file
;;;; Prolog "json-parsing.pl"

#||********************FUNZIONI*DI*USO*GENERALE**********************||#

;;; is-whitespacep/1 verifica se il proprio argomento
;;; è uno spazio bianco, un "Tab" o un "Newline".
(defun is-whitespacep (char)
  (if (characterp char)
      (if (member char '(#\space #\tab #\Newline))
	  T)))

;;; skip-whitespace/1 analizza gli elementi della lista
;;; passata come argomento: appena trova un elemento che non
;;; soddisfa is-whitespacep/1 si ferma e restituisce la lista
;;; contenente i caratteri non ancora esaminati.
(defun skip-whitespace (list-char)
  (if (null list-char)
      nil
    (if (is-whitespacep (car list-char))
	(skip-whitespace (cdr list-char))
      list-char)))

;;; list-trim/1 rimuove gli spazi bianchi, i "Tab" e i "Newline"
;;; all'interno della lista list-char e per farlo si rivolge a
;;; skip-whitespace/1, ribalta la lista una volta terminata la
;;; chiamata, si rivolge nuovamente a skip-whitespace/1 e ribalta
;;; nuovamente la lista. E' una funzione importantissima per il
;;; corretto funzionamento di questo progetto e verrà utilizzata
;;; molto spesso nelle funzioni di supporto a jsonparse/1.
(defun list-trim (list-char)
  (reverse (skip-whitespace (reverse (skip-whitespace list-char)))))

;;; concatenate-all/1 riceve una lista di caratteri, stringhe e nils
;;; e restituisce la stringa che si ottiene concatenando tutti gli
;;; elementi della lista.
(defun concatenate-all (characters)
  (if (null characters)
      nil
    (if (or (characterp (car characters))
	    (null (car characters))
	    (stringp (car characters)))
	(cond
	 ((characterp (car characters))
	  (concatenate 'string (string (car characters))
		       (concatenate-all (cdr characters))))
	 ((stringp (car characters))
	  (concatenate 'string (car characters)
		       (concatenate-all (cdr characters))))
	 (T (concatenate-all (cdr characters))))
      (error "(CONCATENATE-ALL) There is a not convertible element!"
	     (car characters)))))


#||*************************JSONPARSE****************************||#

;;; is-anychar esamina la lista di caratteri list-char finchè
;;; non esauirisce gli elementi oppure non trova un apice
;;; (singolo o doppio) che non è preceduto da un backslash.
;;; In ogni caso restituisce una lista contenente i caratteri
;;; non ancora esaminati e una stringa creata dalla concatenazione
;;; degli elementi esaminati.
(defun is-anychar (list-char &optional (buffer nil))
  (if (null list-char)
      (values nil (reverse (concatenate-all buffer)))
    (let ( (next-char (car list-char)) )
      (cond
       ((and
	 (char= next-char #\\)
	 (or (char= (second list-char) #\")
	     (char= (second list-char) #\')))
	(is-anychar (cdr (cdr list-char))
		    (append (list (second list-char) (first list-char))
			    buffer)))
       ((or (char= next-char #\")
	    (char= next-char #\'))
	(values list-char (reverse (concatenate-all buffer)) ))
       (T (is-anychar (cdr list-char) (cons (first list-char) buffer)))))))

;;; is-string/1 prende in ingresso una lista di caratteri e controlla
;;; se rappresentano un valore String: per farlo si assicura che la lista
;;; contenga una coppia di apici identici (singoli o doppi), lasciando che
;;; tutto ciò che sta al loro interno venga controllato da is-anychar.
;;; A fine chiamata restituisce list-char con i caratteri rimanenti (quindi
;;; non esaminati) insieme alla stringa appena analizzata e creata.
(defun is-string (list-char)
  (if (null list-char)
      (error "(IS-STRING) Someone has not entered any character yet!")
      (let* ((clean-list (list-trim list-char))
	     (vals (multiple-value-list (is-anychar (cdr clean-list))))
	     (rest (first vals))
	     (anychar (second vals))
	     (next-char (car clean-list)))
	(if (or (char= next-char #\")
		(char= next-char #\'))
	    (cond
	      ((null (car rest)) (error "String never closes!"))
	      ((char= (car rest) next-char)
	       (if (null anychar)
		   (values (cdr rest) "")
		   (values (cdr rest)
			   (concatenate-all (list anychar)))))
	      (T (error (concatenate 'string "(IS-STRING) Invalid string!"
				     " It doesn't close with the matched "
				     " quote or it never closes!"))))
	    (error (concatenate 'string
				"(IS-STRING) Invalid string! "
				"It doesn't start with a quote!"))))))

;;; reverse-string/1 controlla se str è una stringa i cui caratteri
;;; soddisfano is-anychar/1, in tal caso lo restituisce delimitato
;;; da doppi apici.
(defun reverse-string (str)
  (if (stringp str)
      (let ((vals (is-anychar (map 'list 'identity str))))
	(if (null vals)
	    (concatenate-all (list
			      #\"
			      str
			      #\"))
	  (error "(REVERSE-STRING) Invalid string!")))
    (error "(REVERSE-STRING) This is not a valid Common Lisp string!")))

;;; is-digit si comporta come is-anychar, cioè esamina ogni
;;; carattere della lista ricevuta e guarda se soddisfano la
;;; funzione digit-char-p/1; non appena termina l'esecuzione
;;; restituisce due liste, la prima contenente i caratteri non
;;; ancora esaminati (NIL se li ha esaminati tutti) e la seconda
;;; contenente invece quelli esaminati.
(defun is-digit (list-char &optional (buffer nil))
  (if (null list-char)
      (values nil (reverse buffer))
    (let ((next-char (car list-char)))
      (if (null (digit-char-p next-char))
	  (values list-char (reverse buffer))
	(is-digit (cdr list-char)
		  (cons next-char buffer))))))

;;; get-number-value/1 verifica se i caratteri all'interno della lista
;;; passatagli rappresentano un numero, intero o decimale. Per fare ciò
;;; invoca is-digit e controlla se il primo elemento della prima delle
;;; due liste che gli vengono restituite è il punto tipico dei numeri
;;; decimali: se lo è allora richiama is-digit per esaminare la parte
;;; decimale del numero (dà errore se is-digit restituisce la stessa
;;; lista che ha ricevuto, il che vuol dire che dopo il punto non c'è
;;; alcuna cifra) e infine restituisce due liste, la prima equivalente
;;; a quella ricevuta all'inizio ma senza i caratteri esaminati e la
;;; seconda contenente proprio questi caratteri. Nel caso che il primo
;;; elemento non sia un punto, verrebbero comunque restituite due liste
;;; con lo stesso criterio del caso precedente.
(defun get-number-value (list-char)
  (let* ((vals (multiple-value-list (is-digit list-char)))
	 (rest* (first vals))
	 (integer (second vals))
	 (is-dot (first rest*)))
    (if (null integer)
	(error "(GET-NUMBER-VALUE) Integer Expected!")
      (cond
       ((null is-dot)
	(values rest* integer))
       ((char= is-dot #\.)
	(let ((vals (multiple-value-list (is-digit (cdr rest*)))))
	  (if (null (second vals))
	      (error "(GET-NUMBER-VALUE) Found dot, but not a decimal part!")
	    (values (first vals)
		    (append
		     integer
		     (list #\.)
		     (second vals))))))
       (T (values rest* integer))))))

;;; is-number/1 si occupa di verificare se i caratteri iniziali
;;; di list-char rappresentano un numero, con o senza segno. Dopo
;;; aver "pulito" la lista con list-trim/1 ed aver effettuato
;;; un controllo sul primo carattere per vedere se è un segno,
;;; cede il controllo a get-number-value: alla fine restituisce
;;; list-char (senza i caratteri esaminati) e il numero analizzato.
(defun is-number (list-char)
  (if (null list-char)
      (error "(IS-NUMBER) Someone has not entered any digit yet!")
    (let* ((clean-list (list-trim list-char))
	   (sign (car clean-list)))
      (if (or (char= #\- sign)
	      (char= #\+ sign))
	  (let ((vals (multiple-value-list
		       (get-number-value (cdr clean-list)))))
	    (values (first vals)
		    (read-from-string
		     (concatenate-all (append (list sign)
					      (second vals))))))
	(let ((vals (multiple-value-list
		     (get-number-value clean-list))))
	  (values (first vals)
		  (read-from-string
		   (concatenate-all (second vals)))))))))

;;; reverse-number/1 verifica se num è un numero: se lo è
;;; allora lo restituisce sotto forma di stringa.
(defun reverse-number (num)
  (if (numberp num)
      (write-to-string num)
    (error "(REVERSE-NUMBER) Invalid number!")))

;;; is-charp/1 restituisce T se il proprio argomento è un carattere
;;; che rappresenta una lettera dell'alfabeto, maiuscola
;;; o minuscola, altrimenti restituisce NIL.
(defun is-charp (char)
  (if (characterp char)
      (let ((code (char-code char)))
	(if (or (and (>= code 65)
		     (<= code 90))
		(and (>= code 97)
		     (<= code 122)))
	    T
	  NIL))
    NIL))

;;; is-char si comporta come is-anychar, cioè controlla se i
;;; primi caratteri di list-char contengono caratteri che
;;; soddisfano is-charp/1 o digit-char-p/1. Al termine della sua
;;; esecuzione restituisce sia la lista contenente i caratteri di
;;; list-char non esaminati sia la lista contenente quelli esaminati.
(defun is-char (list-char &optional (buffer nil))
  (if (null list-char)
      (values nil (reverse buffer))
    (let ((next-char (car list-char)))
      (if (or (is-charp next-char) (digit-char-p next-char))
	  (is-char (cdr list-char) (cons next-char buffer))
	(values list-char (reverse buffer))))))

;;; is-identifier/1 si comporta come is-string/1, cioè verifica
;;; se i primi caratteri di list-char rappresentano un Identifier
;;; rivolgendosi a is-char e assicurandosi che il primo carattere
;;; di clean-list (il risultato di list-trim/1 su list-char) soddisfi
;;; la funzione is-charp/1. Al termine dell'esecuzione restituisce
;;; la lista contenente i caratteri di list-char non ancora esaminati e
;;; una stringa Lisp rappresentante l'Identifier appena esaminato.
;;; (Tale stringa è il risultato della "normalizzazione" degli
;;; Identifier da simbolo a stringa che è stata esplicitamente
;;; richiesta dai docenti. Per evidenziare tale pratica abbiamo
;;; stabilito che is-identifier prima si rivolga a concatenate-all/1
;;; per ricevere l'Identifier sotto forma di stringa, poi lo converta
;;; in simbolo tramite read-from-string/1 e infine lo converta
;;; nuovamente in stringa tramite string/1)
(defun is-identifier (list-char)
  (if (null list-char)
      (error "(IS-IDENTIFIER) Someone has not entered any character yet!")
    (let* ((clean-list (list-trim list-char))
	   (next-char (car clean-list))
	   (vals (multiple-value-list (is-char (cdr clean-list)))))
      (if (is-charp next-char)
	  (values (first vals)
		  (string (read-from-string
			   (concatenate-all
			    (append (list next-char)
				    (second vals))))))
	(error (concatenate 'string "(IS-IDENTIFIER) Invalid identifier!"
			    " It doesn't start with a \"Char\"!"))))))

;;; is-identifierp/1 si assicura che list-char, dopo che è stata
;;; "pulita" con list-trim/1 (clean-list), contenga esclusivamente
;;; caratteri che rappresentano un Identifier, cioè che il primo
;;; carattere di clean-list soddisfi is-charp/1 e gli altri is-char.
(defun is-identifierp (list-char)
  (let ((clean-list (list-trim list-char)))
    (if (is-charp (car clean-list))
	(if (null (first (is-char (cdr clean-list))))
	    T))))

;;; is-pair-attribute/1 controlla se i primi caratteri di list-char
;;; rappresentano un Attribute, quindi una String o un Identifier.
;;; Dopo aver "pulito" list-char con list-trim/1, guarda che cosa
;;; è il primo carattere: se è un apice allora cede il controllo a
;;; is-string/1 mentre se è un carattere alfabetico cede il controllo a
;;; is-identifier/1 (se non è nessuno dei due dà errore). Al termine
;;; della sua esecuzione restituisce una lista contenente i caratteri
;;; di list-char non ancora esaminati e la String/Identifier appena esaminata.
(defun is-pair-attribute (list-char)
  (if (null list-char)
      (error "(IS-PAIR-ATTRIBUTE) Someone has not entered any character yet!")
    (let* ((clean-list (list-trim list-char))
	   (next-char (car clean-list)))
      (cond
       ((or (equal next-char #\")
	    (equal next-char #\'))
	(let* ((vals (multiple-value-list (is-string clean-list)))
	       (clean-rest (list-trim (first vals))))
	  (if
	      (equal (car clean-rest) #\:)
	      (values (cdr clean-rest)
		      (second vals))
	    (error (concatenate 'string "(IS-PAIR-ATTRIBUTE) There is no colon"
				" after pair attribute!")))))
       ((is-charp next-char)
	(let* ((vals (multiple-value-list (is-identifier clean-list)))
	       (clean-rest (list-trim (first vals))))
	  (if
	      (equal (car clean-rest) #\:)
	      (values (cdr clean-rest)
		      (second vals))
	    (error "(IS-PAIR-ATTRIBUTE) There is no colon after pair attribute!"
		   (first clean-rest)))))
       (T (error "(IS-PAIR-ATTRIBUTE) Invalid pair attribute!"))))))

;;; is-value/1 verifica se i primi caratteri di list-char
;;; rappresentano un Value. Per farlo "pulisce" list-char
;;; con list-trim/1 e poi ne esamina il primo carattere:
;;; se è un numero o un segno (+ oppure -) cede il controllo a
;;; is-number/1, se è un apice cede il controllo a is-string/1,
;;; se è una parentesi graffa cede il controllo a is-object/1
;;; e se è una parentesi quadra cede il controllo a is-array/1.
(defun is-value (list-char)
  (if (null list-char)
      (error "(IS-VALUE) Someone has not entered any character yet!")
    (let* ((clean-list (list-trim list-char))
	   (next-char (car clean-list)))
      (cond
       ((or
	 (digit-char-p next-char)
	 (equal #\- next-char)
	 (equal #\+ next-char))
	(is-number clean-list))
       ((or (equal next-char #\")
	    (equal next-char #\'))
	(is-string clean-list))
       ((equal next-char #\{)
	(is-object clean-list))
       ((equal next-char #\[)
	(is-array clean-list))
       (T (error "(IS-VALUE) Invalid Value!"))))))

;;; reverse-value/1 cede il controllo a reverse-object/1 se
;;; val è una lista il cui primo elemento è "jsonobj", a
;;; reverse-array/1 se val è una lista il cui primo elemento
;;; è "jsonarray", a reverse-string/1 se val è una stringa o a
;;; reverse-number/1 se val è un numero.
(defun reverse-value (val)
  (cond
   ((listp val)
    (cond
     ((equal (first val) 'jsonobj)
      (reverse-object val))
     ((equal (first val) 'jsonarray)
      (reverse-array val))
     (T (error (concatenate 'string "(REVERSE-VALUE) Value is not "
			    "neither an array nor an object!")))))
   ((stringp val)
    (reverse-string val))
   ((numberp val)
    (reverse-number val))
   (T (error "(REVERSE-VALUE) Invalid value!"))))

;;; is-pair/1 verifica se i primi caratteri di list-char
;;; rappresentano una Pair. Per farlo si rivolge a is-pair-attribute/1
;;; prima e a is-value/1 poi, infine restituisce la lista contenente
;;; i caratteri di list-char non esaminati dalle due funzioni e la lista
;;; contenente i loro prodotti (Attribute e Value).
(defun is-pair (list-char)
  (let* ((attribute-vals (multiple-value-list (is-pair-attribute list-char)))
	 (attribute-rest (first attribute-vals))
	 (attribute (second attribute-vals))
	 (vals (multiple-value-list (is-value attribute-rest))))
    (values
     (first vals)
     (list attribute (second vals)))))

;;; reverse-pair/1, dopo aver controllato che pair abbia esattamente
;;; due elementi, esegue un controllo sul primo di questi elementi:
;;; se è una stringa che soddisfa is-identifierp/1 allora non effettua
;;; su di esso alcuna modifica, altrimenti lo cede a reverse-string/1
;;; che lo restituirà in attribute. Successivamente reverse-pair/1
;;; concatena attribute (o il first di pair) con il risultato di
;;; reverse-value/1, li separa con i due punti e li restituisce.
(defun reverse-pair (pair)
  (if (and
       (listp pair)
       (= (length pair) 2))
      (if (stringp (first pair))
	  (if (is-identifierp (map 'list 'identity (first pair)))
	      (concatenate-all (list "\""
				     (string (first pair))
				     "\""
				     " : "
				     (reverse-value (second pair))))
	      (let ((attribute (reverse-string (first pair))))
		(concatenate-all (list attribute
				       " : "
				       (reverse-value (second pair))))))
	  (error "(REVERSE-PAIR) Invalid pair attribute!"))
      (error "(REVERSE-PAIR) Invalid pair!")))

;;; is-element verifica se i primi caratteri della lista elements
;;; rappresentano zero, uno o più Elements. E' una funzione ricorsiva
;;; che si appoggia a is-value/1 per determinare la natura degli
;;; Elements, si richiama ricorsivamente quando vede una virgola
;;; (passandosi anche il parametro opzionale is-comma che consente
;;; di sapere se la virgola è stata effettivamente vista) e termina
;;; la propria esecuzione non appena incontra una parentesi quadra
;;; chiusa, salvo quando quest'ultima è preceduta da una virgola ma
;;; non da un Element (informazione fornita da is-comma), caso in cui
;;; viene generato un errore.
(defun is-element (elements  &optional (buffer nil) (is-comma NIL))
  (let* ((clean-list (skip-whitespace elements))
	 (next-char (car clean-list)))
    (if (null clean-list)
	(error "(IS-ELEMENT) There isn't any close square bracket!")
      (if (equal next-char #\])
	  (cond
	   ((equal is-comma T) (error (concatenate 'string "(IS-ELEMENT) "
						   "Found comma, but there "
						   "is not any value "
						   "after it!")))
	   ((equal is-comma nil)
	    (values clean-list (reverse buffer)))
	   (T (error "(IS-ELEMENT) Invalid parameter!" is-comma)))
	(let* ((vals (multiple-value-list (is-value elements)))
	       (clean-rest (list-trim (first vals)))
	       (comma (car clean-rest)))
	  (cond
	   ((equal comma #\,)
	    (is-element (cdr clean-rest) (cons (second vals) buffer) T))
	   ((equal comma #\] )
	    (values clean-rest (reverse (cons (second vals) buffer))))
	   (T (error "(IS-ELEMENT) Invalid element!"))))))))

;;; reverse-element/1 è una funzione ricorsiva che analizza tutti i
;;; Value all'interno di elements servendosi di reverse-value/1
;;; e li restituisce sottoforma di stringa tutti separati dalla virgola.
(defun reverse-element (elements)
  (if (null elements)
      nil
    (let ((value (reverse-value (first elements))))
      (if (null (second elements))
	  value
	(concatenate-all (list
			  value
			  ", "
			  (reverse-element (cdr elements))))))))

;;; is-array/1 verifica se i primi caratteri di list-char
;;; rappresentano un Array. Dopo aver "pulito" list-char
;;; con list-trim/1, verifica se il primo carattere è una
;;; parentesi quadra aperta, si rivolge a is-element
;;; per l'analisi degli eventuali Elements e una volta ripreso
;;; il controllo verifica se c'è la parentesi quadra chiusa:
;;; se è presente allora restituisce la lista contenente i
;;; caratteri di list-char non esaminati e la seconda lista
;;; restituita da is-element ma preceduta da "jsonarray".
(defun is-array (list-char)
  (if (null list-char)
      (error "(IS-ARRAY) Someone has not entered any array yet!")
    (let* ((clean-list (skip-whitespace list-char))
	   (next-char (car clean-list)))
      (if (equal next-char #\[)
	  (let* ((elements-vals (multiple-value-list
				 (is-element (cdr clean-list))))
		 (clean-rest (list-trim (first elements-vals)))
		 (next-char (car clean-rest)))
	    (if (equal next-char #\])
		(if (null (second elements-vals))
		    (values (cdr clean-rest)
			    (list 'jsonarray ))
		  (values
		   (cdr clean-rest)
		   (cons 'jsonarray (second elements-vals))))
	      (error "(IS-ARRAY) Invalid array, not found '~A'" #\])))
	(error "(IS-ARRAY) Invalid array, open squared bracket not found!")))))

;;; reverse-array/1 verifica se array è una lista il cui primo elemento
;;; è "jsonarray", a quel punto affida gli altri elementi a reverse-array/1.
(defun reverse-array (array)
  (if (listp array)
      (if (equal (first array) 'jsonarray)
	  (concatenate-all (list
			    #\[
			    (reverse-element (rest array))
			    #\]))
	(error "(REVERSE-ARRAY) Invalid array!"))
    (error "(REVERSE-ARRAY) This is not a list!")))

;;; remove-pair-duplicates/2 è la funzione a cui is-member si rivolge
;;; per rimuovere i duplicati delle Pairs. E' una funzione ricorsiva
;;; che memorizza solo le Pairs di pair-list che hanno un Attribute
;;; diverso da quello di pair.
(defun remove-pair-duplicates (pair-list pair)
  (if (not (null pair-list))
      (let ((pair-examined (first pair-list)))
	(if (equal (first pair-examined)
		   (first pair))
	    (remove-pair-duplicates (rest pair-list) pair)
	    (cons pair-examined
		  (remove-pair-duplicates (rest pair-list) pair))))))

;;; is-member si comporta esattamente come is-element, con le
;;; seguenti tre differenze:
;;; 1. La parentesi che viene controllata è graffa (non quadra)
;;; 2. Ci si rivolge a is-pair/1 (non a is-value/1)
;;; 3. Dopo aver chiamato is-pair/1, viene effettuata una chiamata
;;;    a remove-pair-duplicates/2 (Necessario per l'eliminazione
;;;    dei duplicati, non c'è nessuna mossa analoga in is-element)
(defun is-member (members  &optional (buffer nil) (is-comma NIL))
  (let* ((clean-list (skip-whitespace members))
	 (next-char (car clean-list)))
    (if (null clean-list)
	(error "(IS-MEMBER) There isn't any close curly bracket!")
      (if (equal next-char #\})
	  (cond
	   ((equal is-comma T)
	    (error (concatenate 'string "(IS-MEMBER) Found comma, but there "
				"isn't any pair after it!")))
	   ((equal is-comma nil)
	    (values clean-list (reverse buffer)))
	   (T (error "(IS-MEMBER) Invalid parameter ~S" is-comma)))
	(let* ((vals (multiple-value-list (is-pair members)))
	       (pair (second vals))
	       (clean-rest (list-trim (first vals)))
	       (comma (car clean-rest))
	       ;; Rimuoviamo i duplicati del pair
	       (clean-buffer (remove-pair-duplicates buffer pair)))
	  (cond
	   ((equal comma #\,)
	    (is-member (cdr clean-rest)
		       (cons pair clean-buffer)
		       T))
	   ((equal comma #\} )
	    (values clean-rest
		    (reverse (cons pair clean-buffer))))
	   (T
	    (error "(IS-MEMBER) Invalid member, ',' or '}' not found"))))))))

;;; reverse-member/1 è una funzione ricorsiva che analizza
;;; tutte le Pairs all'interno di members servendosi di reverse-pair/1
;;; e le restituisce sottoforma di stringa tutte separate dalla virgola.
(defun reverse-member (members)
  (if (null members)
      nil
    (let ((pair (reverse-pair (first members))))
      (if (null (second members))
	  pair
	(concatenate-all (list
			  pair
			  ", "
			  (reverse-member (cdr members))))))))

;;; is-object/1 si comporta esattamente come is-array/1, con le seguenti
;;; tre differenze:
;;; 1. Le parentesi che vengono controllate sono graffe (non quadre)
;;; 2. Ci si rivolge a is-member (non a is-element)
;;; 3. La seconda delle due liste che tale funzione restituisce
;;;    ha come primo elemento "jsonobj" (non "jsonarray")
(defun is-object (list-char)
  (if (null list-char)
      (error "(IS-OBJECT) Someone has not entered any object yet!")
    (let* ((clean-list (list-trim list-char))
	   (next-char (car clean-list)))
      (if (equal next-char #\{)
	  (let* ((members-vals (multiple-value-list
				(is-member (cdr clean-list))))
		 (clean-rest (list-trim (first members-vals)))
		 (next-char (car clean-rest)))
	    (if (equal next-char #\})
		(if (null (second members-vals))
		    (values (cdr clean-rest)
			    (list 'jsonobj))
		  (values
		   (cdr clean-rest)
		   (cons 'jsonobj (second members-vals))))
	      (error "(IS-OBJECT) Invalid object, not found '~A'" #\})))
	(error "(IS-OBJECT) Invalid member, open curly bracket not found!")))))

;;; reverse-object/1 verifica se obj è una lista il cui primo elemento
;;; è "jsonobj", a quel punto affida gli altri elementi a reverse-member/1.
(defun reverse-object (obj)
  (if (listp obj)
      (if (equal (first obj) 'jsonobj)
	  (concatenate-all (list
			    #\{
			    (reverse-member (rest obj))
			    #\}))
	(error "(REVERSE-OBJECT) Invalid object"))
    (error "(REVERSE-OBJECT) This is not a list")))

;;; is-json/1 verifica se i caratteri di list-char rappresentano un
;;; JSON. Dopo aver "pulito" list-char con list-trim/1, ne esamina il
;;; primo carattere: se è una parentesi graffa aperta, cede il controllo
;;; a is-object/1 mentre se è una parentesi quadra aperta cede il controllo
;;; a is-array/1 (altrimenti dà errore).
(defun is-json (list-char)
  (if (null list-char)
      (error "(IS-JSON) Someone has not entered any character yet!")
    (let* ((clean-list (list-trim list-char))
	   (next-char (car clean-list)))
      (cond
       ((equal next-char #\{)
	(is-object clean-list))
       ((equal next-char #\[)
	(is-array clean-list))
       (T (error "(IS-JSON) Invalid json!"))))))

;;; reverse-json/1 in base a ciò che è il primo elemento di json
;;; cede il controllo a reverse-object/1 (first json = jsonobj) o
;;; a reverse-array/1 (first json = jsonarray).
(defun reverse-json (json)
  (cond
   ((equal (first json) 'jsonobj)
    (reverse-object json))
   ((equal (first json) 'jsonarray)
    (reverse-array json))
   (T (error "(REVERSE-JSON) Invalid JSON"))))

;;; jsonparse/1 riceve una stringa (json), crea una lista
;;; (list-char) in cui inserisce tutti i caratteri che compongono
;;; la stringa ricevuta e la cede a is-json/1. Una volta ripreso il
;;; controllo, effettua una verifica sulle liste che is-json/1 ha
;;; restituito: se la prima lista è vuota allora restituisce la
;;; seconda (tutti i caratteri di list-char sono stati esaminati
;;; e consumati perciò json è stata riconosciuta come un
;;; JSON valido), altrimenti dà errore.
(defun jsonparse (json)
  (let* ((list-char (map 'list 'identity json))
	 (clean-list (list-trim list-char)))
    (if (null clean-list)
	nil
      (let ((vals (multiple-value-list (is-json clean-list))))
	(if (null (first vals))
	    (second vals)
	  (error "(JSONPARSE) Invalid JSON object!"))))))

;;; reverse-jsonparse/1 semplicemente cede il controllo a reverse-json/1.
(defun reverse-jsonparse (json)
  (reverse-json json))


#||*****************************JSONGET*****************************||#

;;; discover/2 cede il controllo a search-obj/2 se il primo elemento
;;; di json è "jsonobj" o a search-array/2 se il primo elemento è
;;; "jsonarray". Si occupa inoltre di verificare la correttezza
;;; sintattica di field tramite symbolp/1, stringp/1 e integerp/1.
(defun discover (json field)
  (cond
   ((equal (first json) 'jsonobj)
    (if (or (symbolp field)
	    (stringp field))
	(search-obj (rest json) field)
      (error "(DISCOVER) Invalid search field for JSONobj!")))
   ((equal (first json) 'jsonarray)
    (if (integerp field)
	(search-array (rest json) field)
      (error "(DISCOVER) Invalid search field for JSONarray!")))
   (T (error "(DISCOVER) Invalid JSON object"))))

;;; search-obj/2 è una funzione ricorsiva che restituisce nil
;;; se pairs è nil, altrimenti si appoggia a search-pair/2
;;; passandogli il primo elemento di pairs: se il risultato
;;; di search-pair/2 è diverso da nil allora lo restituisce
;;; altrimenti effettua una chiamata ricorsiva con gli altri
;;; elementi di pairs.
(defun search-obj (pairs field)
  (if (null pairs)
      NIL
    (let ((pair-val (search-pair (car pairs) field)))
      (if (null pair-val)
	  (search-obj (rest pairs) field)
	pair-val))))

;;; search-array/2 restituisce l'elemento di elements alla posizione
;;; index. Se elements è nil allora restituisce nil, se index è
;;; maggiore della lunghezza di elements allora dà errore.
(defun search-array (elements index)
  (if (null elements)
      NIL
    (if (> index
	   (length elements))
	(error "(SEARCH-ARRAY) Invalid JSONarray index given!")
      (nth index elements))))

;;; search-pair/1 restituisce il secondo elemento di pair se
;;; il primo elemento è uguale a field, altrimenti restituisce nil.
(defun search-pair (pair field)
  (if (equal (first pair) field)
      (second pair)
    NIL))

;;; is-jsonobjp/1 restituisce T se json è una lista
;;; il cui primo elemento è "jsonobj".
(defun is-jsonobjp (json)
  (if (listp json)
      (if (equal (first json)
		 'jsonobj)
	  T)))

;;; is-jsonarrayp/1 restituisce T se json è una lista
;;; il cui primo elemento è "jsonarray".
(defun is-jsonarrayp (json)
  (if (listp json)
      (if (equal (first json)
		 'jsonarray)
	  T)))

;;; is-json-elementp/1 restituisce T se json soddisfa
;;; is-jsonobjp/1 o is-jsonarrayp/1.
(defun is-json-elementp (json)
  (if
      (or (is-jsonobjp json)
	  (is-jsonarrayp json))
      T))

;;; next-element/2 verifica la correttezza sintattica di json
;;; tramite is-json-elementp/1 e in base alla lunghezza di fields
;;; si comporta nei seguenti modi: se la lunghezza è 1 allora
;;; cede il controllo a discover mentre se è maggiore di 1 si
;;; richiama ricorsivamente passandosi come argomenti il prodotto
;;; di discover e i restanti elementi di fields.
(defun next-element (json fields)
  (cond
   ((and(= (length fields) 1)
	(is-json-elementp json))
    (discover json (first fields)))
   ((and(> (length fields) 1)
	(is-json-elementp json))
    (next-element (discover json (first fields))
		  (rest fields)))
   (T (error (concatenate 'string "(NEXT-ELEMENT) JSON object or fields' "
			  "length are not correct!")))))

;;; jsonget/2 verifica la correttezza sintattica di json con
;;; reverse-jsonparse/1 e eventualmente trasforma search-entry
;;; in una lista (con list), dopodichè cede il controllo a
;;; next-element/2 e ne inserisce il risultato in val: se val
;;; è diverso da nil allora lo restituisce, altrimenti dà errore.
(defun jsonget (json search-entry)
  (let* ((verified-json (reverse-jsonparse json))
	 (fields (if (atom search-entry)
		     (list search-entry)
		   search-entry)))
    (let ((val (next-element json fields)))
      (if (null val)
	  (error "(JSONGET) Search has not found any value from fields list")
	val))))


#||******************************JSONWRITE*****************************||#

;;; jsonwrite/2 converte json in json-string tramite reverse-jsonparse/1,
;;; poi si rivolge a with-open-file per creare un file (o modificare
;;; un file esistente) dal nome path e scrivere json-string al suo interno.
;;; Al termine dell'esecuzione restituisce il file appena manipolato.
(defun jsonwrite (json path)
  (let ((json-string (reverse-jsonparse json)))
    (progn
      (with-open-file (stream path :direction :output :if-exists
			      :supersede :if-does-not-exist :create)
		      (format stream json-string))
      path)))


#||******************************JSONLOAD******************************||#

;;; jsonload/1 riceve in ingresso il nome di un file e ne estrae
;;; il contenuto tramite file-to-string/1. Tale contenuto viene
;;; poi inserito in json-string e passato a jsonparse/1.
(defun jsonload (path)
  (let ((json-string (file-to-string path)))
    (jsonparse json-string)))

;;; file-to-string/1 si rivolge a with-open-file per generare uno stream
;;; dal file in input (path) e passarlo a read-til-end/1. Se path non
;;; corrisponde ad alcun file esistente, file-to-string/1 genera un errore
;;; (:if-does-not-exist :error).
(defun file-to-string (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
		  (read-til-end stream)))

;;; read-til-end/1 è una funzione ricorsiva che riceve uno stream
;;; di input relativo un file già aperto con i permessi di lettura
;;; (stream). La funzione fondamentale su cui read-til-end/1 si basa
;;; è read-line, di cui si parlerà più dettagliatamente in seguito e
;;; da cui riceve una stringa vals: se tale stringa è nil allora viene
;;; restituita da sola, altrimenti viene concatenata insieme al risultato
;;; della chiamata ricorsiva.
(defun read-til-end (stream)
  (let ((vals (read-line stream nil)))
    (if (null vals)
	vals
      (concatenate 'string vals (read-til-end stream)))))

#|| COMMENTO SU READ-LINE

read-line è una funzione già definita in Lisp. Essa legge e restituisce
una linea di file alla volta e possiede una "testina" che si muove lungo
tutti i caratteri della linea in esame. Muovendosi all'interno del file,
tale testina consente a read-til-end/1 quando effettua la chiamata
ricorsiva di far esaminare a read-line la linea successiva del file:
infatti la testina punterà alla prossima linea da leggere.

A read-line vengono passati lo stream ricevuto da read-til-end/1 e un
valore nil. Tale valore è in riferimento alla keyword :eof-error-p,
inizializzata con questo valore per ottenere il comportamento descritto
dalla seguente frase presa dalla sezione di Hypersec relativa read-line:
"If eof-error-p is false and the end of file for input-stream
is reached before any characters are read, eof-value is returned
as the line."
In sostanza, :eof-error-p viene inizializzata a nil per garantire che,
in caso di linee vuote non venga generato un errore, read-line non genera un errore,
ma restituisca nil.
||#
