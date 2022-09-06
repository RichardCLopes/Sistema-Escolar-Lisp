(DEFUN CMP1 (DISCIPLINAS BD)

	(if(null bd)
		nil
		(if(equal DISCIPLINAS(caar bd))
			(car bd)
			(cmp DISCIPLINAS (cdr BD))
		)
	)
	;(cmp '"APC" BD)
)
(DEFUN CMP2 (DISCIPLINAS BD)
	
		(if(equal(car DISCIPLINAS)(caar bd))
			't
			(CMP2 DISCIPLINAS (cdr bd))
		)
	
	;; (CMP2 '"APC" bd)
)
(DEFUN CMP (DISCIPLINAS BD NOVA)
	
		(if (null DISCIPLINAS);;VERIFICA SE O CAR É NULL
		      nil
		(if(equal(car DISCIPLINAS)(caar bd));;VERIFICA SE A IMPRESSAO DA DISCIPLINA É IGUAL
			(append (cdr NOVA)(cdr DISCIPLINAS))
			(cmp (cdr DISCIPLINAS) bd (append NOVA (cons(car DISCIPLINAS) nil)));;RECURSAO
		)
		)
	
)

(DEFUN VERIFICA_ALUNO (ALUNO BD_AL);;aux para atualiza
	
	(if(null BD_AL)
		nil
		(if(equal ALUNO (car BD_AL))
		't
		(VERIFICA_ALUNO ALUNO (cdr BD_AL)))
	)

)
(DEFUN ATUALIZA(ALUNOS BD);;atualiza e reconstroi

	(if (VERIFICA_ALUNO (car ALUNOS) (caadar bd));;verifica se já esta cadastrado
	(ATUALIZA (cdr ALUNOS) BD)
	(append (list
				(list (caar bd) 
					(list(append(caadar bd)ALUNOS)) 
					(caddar bd))
			)(cdr bd))	
	)
)

(DEFUN MATRICULAR (ALUNOS DISCIPLINAS BD)
	
		(if (equal(car DISCIPLINAS)(caar bd));;verifica se materia já esta registrada
			(ATUALIZA ALUNOS BD);;atualiza alunos(evitando repetição de alunos)
			(if(null bd);;acrescenta caso não tiver banco ou estiver no fim	
				(list(list (car DISCIPLINAS)(list ALUNOS)'P))
				
				(append (list (list (caar bd) (cadar bd) (caddar bd)))
					(MATRICULAR ALUNOS DISCIPLINAS (cdr BD)))  			
			)
		)
		
		
)
(DEFUN MUDA_DISCIPLINA (ALUNOS DISCIPLINAS BD);;(anda com disciplina)
		
		(if (null (cdr disciplinas))
		(MATRICULAR ALUNOS DISCIPLINAS BD)
		(MUDA_DISCIPLINA ALUNOS (cdr DISCIPLINAS) (MATRICULAR ALUNOS DISCIPLINAS BD))
		)

)
;;/////////////////////////////////FUNÇÕES PROJETO//////////////////////////
(DEFUN ALUNOS? (BD);;TODOS ALUNOS CADASTRADOS 

	(if (null bd)
	nil
	(if(null (caar bd));;verifica se está vinc ulado a alguma disciplina
		(ALUNOS? (cdr BD)) 
		(append (caadar bd) (Alunos? (cdr BD)))
	))
)	


(DEFUN PROFESSORES? (BD);;TODOS PROFESSORES CADASTRADOS 

	(if (null bd)
	nil
	(if (null (caddar bd));;verifica se está vinculado algum professor
		(PROFESSORES? (cdr BD))
		(cons (caar bd) (DISCIPLINAS? (cdr BD)))
	))


)

(DEFUN DISCIPLINAS? (BD);;TODAS DISCIPLINAS CADASTRADAS 

	(if (null bd)
	nil
	(if (and (null (caadar bd))(null (caddar bd)));;verifica se está vinculado a algum aluno ou professor
		(DISCIPLINAS? (cdr BD))
		(cons (caar bd) (DISCIPLINAS? (cdr BD)))
	))


)

(DEFUN MATRICULADOS? (DISCIPLINA BD);;ALUNOS MATRICULADOS EM UMA DADA DISCIPLINA
	(if (null bd)
	nil
	(if (equal DISCIPLINA(caar bd));;verifica se está vinculado a algum aluno ou professor
		(caadar bd)
		(MATRICULADOS? DISCIPLINA (cdr BD))	
	))
)

(DEFUN VINCULADOS? (DISCIPLINA BD);;NOME DE TODOS PROFESSORES EM UMA DISCIPLINA

(if (null bd)
	nil
	(if (equal DISCIPLINA (caar bd));;verifica se está vinculado a algum aluno ou professor
		(caddar bd)
		(VINCULADOS? DISCIPLINA (cdr BD))	
	))

)

(DEFUN BUSCA_ALUNO (ALUNO AUX BD) ;;auxiliar de CURSA?

	(if (null bd)
	nil
	(if (equal ALUNO (car AUX));percorre dentre alunos de uma disciplina
		(cons(caar bd)(BUSCA_ALUNO ALUNO (caadar (cdr BD)) (cdr BD))) ;;muda de disciplinas
		(if(null (cdr AUX))
			(BUSCA_ALUNO ALUNO (caadar (cdr bd)) (cdr BD));;muda de disciplinas
			(BUSCA_ALUNO ALUNO (cdr(caadar bd)) BD) ;;muda de (aluno) de uma disciplina
		)
	))
)
(DEFUN CURSA? (ALUNO BD);;DISCIPLINAS CURSADAS POR UM ALUNO 

	(BUSCA_ALUNO ALUNO (caadar BD) BD)
)

(DEFUN BUSCA_PROFESSOR (PROFESSOR AUX BD) ;;auxiliar de MINISTRA?

	(if (null bd)
	nil
	(if (equal PROFESSOR (car AUX));percorre dentre alunos de uma disciplina
		(cons(caar bd)(BUSCA_PROFESSOR PROFESSOR (caddar (cdr BD)) (cdr BD))) ;;muda de disciplinas
		(if(null (cdr AUX))
			(BUSCA_PROFESSOR PROFESSOR (caddar (cdr bd)) (cdr BD));;muda de disciplinas
			(BUSCA_PROFESSOR PROFESSOR (cdr(caddar bd)) BD) ;;muda de (aluno) de uma disciplina
		)
	))
)
(DEFUN MINISTRA? (PROFESSOR BD);;DISCIPLINAS CURSADAS PELO PROFESSOR

	(BUSCA_PROFESSOR PROFESSOR (caddar BD) BD)
)
;;///////////////////////////FERRAMENTAS//////////////////////
(DEFUN COLOCA(ALUNOS DISCIPLINAS BD)

		(append (cdr bd)(cons
					(cons DISCIPLINAS (list 
							(cons(append(caadar bd) ALUNOS)nil ) (cons'(P) nil)))
				nil)		
		)

)

(DEFUN VINCULAR (PROFESSORES DISCIPLINAS BD)	
	(if(null bd)
	 nil
		(if (equal (car DISCIPLINAS)(caar bd));;verifica se materia já esta registrada
				(append (list(list (caar bd)(caadar bd)(list(caddar BD) PROFESSORES))) (cdr bd))
				(append (list (list (caar bd) (cadar bd) (caddar bd)))
					(VINCULAR PROFESSORES DISCIPLINAS (cdr BD)))  			
			)	
	)	
)

;(DEFUN VINCULAR (PROFESSOR DISCIPLINAS BD)


			

	;(if(null BD )
	;	(car BD)
	;	(if (CMP2 DISCIPLINAS BD);;verifica se materia já esta registrada
	;	(if( null (cdr bd)
	;	(append (car bd) (list (list (caar bd) (caadar bd) (list (caddar bd) PROFESSOR))))
	;(append	(list(list (caar bd)(caadar bd)(list(caddar BD) PROFESSOR))) (cdr BD)));;NAO APAGA MAIS BD
	;)))
			
    ;; (VINCULAR PROFESSOR DISCIPLINAS (cdr BD))


;)

;;(cons(cons(cons DISCIPLINAS NIL ) ALUNOS) (CDR BD)) FUNCIONANDO 1
;(cons(cons(cons DISCIPLINAS Lista (crd alunos) ) ALUNOS) NIL)
;(cons DISCIPLINAS cons(cons(ALUNOS NIL)cons(PROFESSORES NIL)
;(append (cons DISCIPLINAS nil) (cons DISCIPLINAS nil  )) faz alocação  da mesma lista de disciplina
;;(append (cons (cons(car DISCIPLINAS) nil) nil) (cons (cdr DISCIPLINAS) nil  ))faz alocação  de outra disciplina
;(if (null(cdr DISCIPLINAS))
;			0
;			(MATRICULAR (ALUNOS (cdr DISCIPLINAS) BD))
;			
;)

;;(cons(cons 'D (cons (cons 'ALUNOS NIL ) (cons(cons'PROFESSORES nil)nil))) 'BD)



; (DEFUN CMP (DISCIPLINAS BD)


	; (if(equal DISCIPLINAS(caar bd))
	; '(igual)
	; '(diferente)
	; )
; )


