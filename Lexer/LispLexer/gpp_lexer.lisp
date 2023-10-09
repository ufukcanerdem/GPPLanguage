;UFUKCAN ERDEM
;1901042686
;CSE 341 FALL 2022 HW#2


(defun DFA (arg1)
	(cond
		((equal "**" arg1) (concatenate 'string "(\"" arg1 "\" \"DBL_MULT\")"))
		((equal "-" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_MINUS\")"))
		((equal "+" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_PLUS\")"))
		((equal "*" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_MULT\")"))
		((equal "/" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_DIV\")"))
		((equal "(" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_OP\")"))
		((equal ")" arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_CP\")"))
		((equal "," arg1)  (concatenate 'string "(\"" arg1 "\" \"OP_COMMA\")"))
		((equal "\"" arg1) (concatenate 'string "(\"" arg1 "\" \"OP_OC\")"))
		((is-it-keyword arg1)  (concatenate 'string "(\"" arg1 "\" \"KW_" (string-upcase arg1) "\")"))
		((is-it-integer arg1)  (concatenate 'string "(\"" arg1 "\" \"VALUEI\")") )
		((is-it-integerf arg1)  (concatenate 'string "(\"" arg1 "\" \"VALUEF\")") )
		((is-it-id arg1)  (concatenate 'string "(\"" arg1 "\" \"IDENTIFIER\")"))
		((is-it-str arg1)  (concatenate 'string "(" arg1 " \"VALUESTR\")"))
		(t nil)
	)
)

(defun is-it-operator (op) 
	(setf op_list 
		(list 
			"+" 
			"-" 
			"/" 
			"(" 
			"*" 
			")" 
			"**"
		)
	)
	(reduce #'(lambda (x y) (or x (equal op y))) op_list :initial-value nil)
)

(defun is-it-keyword (key) 
	(setf keywords (list "and" "or" "not" "less" "nil" "list" "equal" "append" "concat" "load"
					"set" "deffun" "for" "disp" "true" "false" "if" "exit")
	)
	(reduce #'(lambda (x y) (or x (equal key y))) keywords :initial-value nil)
)

(defun is-it-integer (integer)

	(setq letters "0123456789-.")
	(setq result t)

	(if (< 0 (length integer))
		(loop for c across integer do
			(if (not (position c letters))
				(setq result nil)
			)
		)
		(setq result nil)
	)
	result
)

(defun is-it-integerf (integerf)

	(setq letters "f0123456789-.")
	(setq result t)

	(if (< 0 (length integerf))
		(loop for c across integerf do
			(if (not (position c letters))
				(setq result nil)
			)
		)
		(setq result nil)
	)
	result
)

(defun is-it-id (identifiers)
	(setq letters "abcdefghijklmnopqrstuvwxyz
			0123456789
			ABCDEFGHIJKLMNOPRSTUVWXYZQ"
	)
	(setq outcome  t)
	(if (< 0 (length identifiers))
		(loop for c across identifiers do
			(if (not (position c letters))
				(setq outcome nil)
			)
		)
		(setq outcome nil)
	)
	outcome
)

(defun is-it-str (stringval)
	(setq letters 	"\"
					abcdefghijklmnopqrstuvwxyz
					0123456789
					ABCDEFGHIJKLMNOPRSTUVWXYZQ
					"
	)
	(setq outcome  t)
	(if (< 0 (length stringval))
		(loop for c across stringval do
			(if (not (position c letters))
				(setq outcome nil)
			)
		)
		(setq outcome nil)
	)
	outcome
)

(defun read-file (filename)
	(concatenate 'string 
		(with-open-file (stream filename)
			(loop for line = (read-char stream nil)
				while line
				collect line
			)
		)
	)
)

(defun gppinterpreter (codes)
	(defvar temp "")
	(defvar token nil)
	(defvar instruction "")
	(defvar inst1 nil)
	(defvar inst2 nil)

	(loop for c across codes do
		(cond ((or (equal inst1 T)(char= #\; c ))
				(setq inst1 T)
				(cond  ((or (equal inst2 T)(char= #\; c ))
						(setq inst2 T)
						(cond ((not(char= c #\Newline))
							(setq instruction (concatenate 'string instruction (string c))))
							(t (setq inst1 nil)(setq inst2 nil)
							(setq token (append token '("COMMENT"))))))))
		)
		(if (and (is-it-operator (string c)) (not(equal inst1 T)))
				(cond ((DFA temp)
					(setq token (append token (list (DFA temp)))) 
					(setq temp "")))
		)
		(if (and (not (or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)))(not(equal inst1 T)))
			(setq temp (concatenate 'string temp (string c)))
		)
		(if (and (or (is-it-operator (string c)) (char= c #\Space) (char= c #\Newline) (char= c #\Tab)) (not(equal inst1 T)))
			(cond ((DFA temp)
				(setq token (append token (list (DFA temp))))
				(setq temp ""))
			)
		)	
	)
	(if (< 0 (length temp))
		(cond ((DFA temp)
			(setq token (append token (list (DFA temp)))))
		)
	)
	
	(format t(princ-to-string token) )
	(terpri)

	(setq temp "")
	(setq token nil)
	(setq instruction "")
	(setq inst1 nil)
	(setq inst2 nil)
)

(defun repeat ()
	(print "REPL HAS STARTED. ENTER YOUR CODE TO TERMINAL. TYPE 'finish' to terminal for exit from the program")
	(loop
		(terpri)
		(format t "->")
		(setq line (read-line))
		(if (not (string= line ""))
			(mapcar 'write-line (gppinterpreter line))
		)
		(when (string= line "finish")(return 0))
	)
)

(defun start ()
	(defvar filename NIL)
	(if (= 1 (length *args*))
		(setq filename (nth 0 *args*))
	)
	(if (not filename)
		(repeat)
		(mapcar 'write-line (gppinterpreter  (read-file filename)))
	)
)

(start)