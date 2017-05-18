;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Funcoes hash table;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;; hash table functions "copy-hash-table", "hash-table-vals" and "hash-table-keys" implementation were based on the author's,vseloved, file hash-table.lisp
;; hash-table.lisp can be found in https://github.com/vseloved/rutils/blob/master/core/hash-table.lisp

	(defun copy-hash-table (ht &key key test size
		                        rehash-size rehash-threshold)
	;;Returns a copy of hash table HT, with the same keys and values
	;;The copy has the same properties as the original, unless overridden by the keyword arguments

	;;Before each of the original values is set into the new hash-table,
	;;KEY is invoked on the value. As KEY defaults to IDENTITY a shallow copy is returned by default.
	  (setf key (or key 'identity))
	  (setf test (or test (hash-table-test ht)))
	  (setf size (or size (hash-table-size ht)))
	  (setf rehash-size (or rehash-size (hash-table-rehash-size ht)))
	  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold ht)))
	  (let ((copy (make-hash-table :test test :size size
			               :rehash-size rehash-size
			               :rehash-threshold rehash-threshold)))
	    (progn	(cond ((equal key 'copia-moeda-aux) 						;caso a operacao seja uma copia da hash-table de moeda
					(maphash (lambda (pos moeda_copiar) 
						(setf	(gethash pos copy)
							(make-moeda	:valor (moeda-valor moeda_copiar)
									:id_fio (mapcar #'(lambda(x) x) 
											(moeda-id_fio moeda_copiar))))) 
					ht))
				 ((equal key 'copia-fio-aux) 						;caso a operacao seja uma copia da hash-table de fio
					(maphash (lambda(id fio_copiar)
							(setf	(gethash id copy) 
								(cria-fio	(car fio_copiar) 
										(fio-origem fio_copiar)
										(fio-destino fio_copiar))))
						 ht))
				(T (maphash (lambda (k v) 						;caso nao seja especificada a operacao cada posicao e um ponteiro para a posicao da hash-table original 
					       (setf (gethash k copy) (funcall key v)))
					ht)))
	    		copy)))

	(defun hash-table-vals (ht)
	  ;;Return a list of values of hash-table HT.
	  (loop :for v :being :the :hash-values :of ht
	     :collect v))
	(defun hash-table-keys (ht)
	  ;Return a list of keys of hash-table HT.
	  (loop :for k :being :the :hash-keys :of ht
	     :collect k))








;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Listas;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;; funcao que recebe um elemento e adiciona-o no ultimo lugar da lista
	
	(defun insere-ultimo (lst el)
		(if(null lst)
			(cons el lst)
			(cons (car lst) (insere-ultimo (rest lst) el))))


;;funcao que remove um elemento da lista
		
	(defun remove_key_list(key lst)
		(cond	((null lst) ()) 
			((eql (first lst) key) (rest lst))
			(T (cons (first lst) (remove_key_list key (rest lst))))))


		

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; TIPOS DE DADOS;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;




;;
;;posicao
;;

	(defun cria-posicao (l c) (cons l c))  

	(defun posicao-linha (p) (car p))

	(defun posicao-coluna(p) (cdr p))

	(defun posicao-p(p) (and (consp p) (>= (car p) 0) (>= (cdr p) 0)))

	(defun posicoes-iguais-p (p1 p2)(and (=(car p1) (car p2))(= (cdr p1) (cdr p2))))




;;
;;fio
;;

	(defun cria-fio(i pos_or pos_dest) (cons i (cons pos_or pos_dest)))

	(defun fio-id (f) (first f))

	(defun fio-origem (f) (car (cdr f)))

	(defun fio-destino (f) (cdr (cdr f)))

	(defun fio-p (f) (and (consp f) (> (car f) 0) (posicao-p(car(cdr f))) (posicao-p(cdr(cdr f)))))




;;
;;tabuleiro
;;

;; estrutura de tabuleiro composta por numero de max linhas e colunas id do prox fio a ser criado hash table de moedas e uma hash table de fios 

	(defstruct tab lin col (new_id 1) (m (make-hash-table :test 'equal)) (f (make-hash-table :test 'equal)))


;;estrutura de moedas composta pelo valor da moeda uma lista de todos os fios ligados a moeda
	
	(defstruct moeda valor id_fio)

	
;;funcao que incrementa o id de fio do tabuleiro

	(defun tabuleiro-id-inc(tb)(incf (tab-new_id tb)))


	(defun cria-tabuleiro (l c) (make-tab :lin l :col c))


	(defun copia-tabuleiro (tb) (make-tab 
					:lin (tab-lin tb) 
					:col (tab-col tb) 
					:new_id (tab-new_id tb) 
					:m (copy-hash-table (tab-m tb) :key 'copia-moeda-aux :test 'equal)
					:f (copy-hash-table (tab-f tb) :key 'copia-fio-aux)))


	(defun tabuleiro-linhas(tb) (tab-lin tb))


	(defun tabuleiro-colunas(tb) (tab-col tb))


	(defun tabuleiro-fios(tb) (hash-table-vals (tab-f tb)))

	(defun tabuleiro-fios-id(tb) ;(hash-table-keys (tab-f tb))
					(mapcar #'(lambda(f)(fio-id f)) (tabuleiro-fios tb))				
					)

	(defun tabuleiro-fio-com-id(tb int)(gethash int (tab-f tb)))


	(defun tabuleiro-fios-posicao(tb pos)
		(if(not (null (gethash pos (tab-m tb)))) 
			(tabuleiro-fios-posicao-aux tb (moeda-id_fio (gethash pos (tab-m tb))) )))
	

;;funcao que dado um tabuleiro e uma lista de ids de fios retorna uma lista de fios  
	
	(defun tabuleiro-fios-posicao-aux (tb lst)
			(if(null lst) 
				()
				(cons(tabuleiro-fio-com-id tb (car lst))
					(tabuleiro-fios-posicao-aux tb (rest lst)))))
	

	(defun tabuleiro-moeda-posicao(tb pos)(let((coin (gethash pos (tab-m tb))))
		(cond(coin (moeda-valor coin))
			(T coin))))


	(defun tabuleiro-total-moedas(tb)(let((total 0))
						(progn (dolist(pos (hash-table-keys (tab-m tb)))
							(setf total (+ total (tabuleiro-moeda-posicao tb pos))))
							total)))


	(defun tabuleiro-adiciona-fio! (tb pos_o pos_d) 	
		(setf (gethash (tab-new_id tb) (tab-f tb)) 
			(cria-fio (tab-new_id tb) pos_o pos_d))
		(setf (moeda-id_fio (gethash pos_o (tab-m tb))) 
			(cons (tab-new_id tb)  (moeda-id_fio (gethash pos_o (tab-m tb)))))
		(setf (moeda-id_fio (gethash pos_d (tab-m tb))) 
			(cons (tab-new_id tb)  (moeda-id_fio (gethash pos_d (tab-m tb)))))
		(tabuleiro-id-inc tb))


	(defun tabuleiro-adiciona-moeda-posicao!(tb pos int) 
		(setf (gethash pos (tab-m tb)) (make-moeda :valor int)))


	(defun tabuleiro-remove-fio-com-id!(tb id_r) 
		(let ((f (tabuleiro-fio-com-id tb id_r)))
			(when f 
				(setf	(moeda-id_fio (gethash(fio-origem f)(tab-m tb)))
					(remove_key_list id_r (moeda-id_fio (gethash (fio-origem f) (tab-m tb)))))
				(setf	(moeda-id_fio (gethash(fio-destino f)(tab-m tb)))
					(remove_key_list id_r (moeda-id_fio (gethash (fio-destino f) (tab-m tb)))))
					(remhash id_r (tab-f tb)))))


	(defun tabuleiro-remove-moeda-posicao!(tb pos)(remhash pos (tab-m tb)))  




;;;
;;; Jogo
;;;
	
	(defstruct jog tabuleiro (pontuacao (cons 0 0)) (jogador 1) (fios_removidos ()))


	(defun cria-jogo (tb) (make-jog :tabuleiro tb))


	(defun jogo-tabuleiro (j) (jog-tabuleiro j))


	(defun jogo-jogador (j) (jog-jogador j))


	(defun jogo-pontos-jogador1 (j) (car (jog-pontuacao j)))


	(defun jogo-pontos-jogador2 (j) (cdr (jog-pontuacao j)))


	(defun jogo-historico-jogadas (j) (jog-fios_removidos j))


	(defun copia-jogo (j)	(make-jog	:tabuleiro (copia-tabuleiro (jogo-tabuleiro j)) 
						:pontuacao (cons (jogo-pontos-jogador1 j) (jogo-pontos-jogador2 j)) 
						:jogador (jogo-jogador j) 
						:fios_removidos (mapcar	#'(lambda(x)x) 
									(jogo-historico-jogadas j))))
	

	
	
	(defun jogo-aplica-jogada! (j id_f)
		(let*(	(tb (jogo-tabuleiro j))
			(fio_rem (tabuleiro-fio-com-id tb id_f))  ;fio a remover
			(pos_origem (fio-origem fio_rem))
			(pos_destino (fio-destino fio_rem))
			(j_outravez NIL))	;flag que indica se o jogador actual joga outra vez
				(when fio_rem						
					(setf (jog-fios_removidos j) (insere-ultimo (jogo-historico-jogadas j) id_f)) 	;adiciona a jogada actual ao historico
					(tabuleiro-remove-fio-com-id! tb id_f) 					;remove fio do tabuleiro
					(when (null (tabuleiro-fios-posicao tb pos_origem))  			;se a moeda de origem do fio removido nao tiver mais fios
							(if (= (jogo-jogador j) 1) 				;da os pontos ao jogador que esta a jogar
								(setf	(car (jog-pontuacao j))
									(+ (car (jog-pontuacao j)) (tabuleiro-moeda-posicao tb pos_origem)))
								(setf	(cdr (jog-pontuacao j))
									(+ (cdr (jog-pontuacao j)) (tabuleiro-moeda-posicao tb pos_origem))))		
							(tabuleiro-remove-moeda-posicao! tb pos_origem)		;remove moeda
							(setf j_outravez T))					;como removeu moeda joga outra vez
					(when (null (tabuleiro-fios-posicao tb pos_destino)) 			;se a moeda de destino do fio removido nao tiver mais fios
							(if (= (jogo-jogador j) 1) 				;da os pontos ao jogador
								(setf	(car (jog-pontuacao j))
									(+ (car (jog-pontuacao j)) (tabuleiro-moeda-posicao tb pos_destino)))
								(setf	(cdr (jog-pontuacao j))
									(+ (cdr (jog-pontuacao j)) (tabuleiro-moeda-posicao tb pos_destino))))		
							(tabuleiro-remove-moeda-posicao! tb pos_destino)	;remove moeda
							(setf j_outravez T))					;activa flag
					(if (null j_outravez) 							;decide que faz a proxima joga
						(if (= (jogo-jogador j) 1) 
							(setf (jog-jogador j) 2)
							(setf (jog-jogador j) 1))))))


	(defun jogo-terminado-p (j) (null (tabuleiro-fios (jogo-tabuleiro j))))




;;;
;;;Problema
;;;

; deve haver mais coisas aqui, para ja so vejo isto
(defstruct problema
	estado-inicial
	(jogador #'(lambda(estado) (jogo-jogador estado)))
	(accoes #'(lambda(estado) (accoes estado)))
	(resultado #'(lambda(estado accao) (resultado estado accao)))
	(teste-corte-p #'(lambda(estado i) (teste-terminal-p estado i)))
	(funcao-avaliacao #'(lambda(estado i) (utilidade estado i) ))
	(historico-accoes  #'(lambda(estado) (jogo-historico-jogadas estado)))
	(chave-equivalencia #'(lambda(estado) 
				(list	(tabuleiro-fios (jogo-tabuleiro estado))
					(jogo-historico-jogadas estado) 
					(jogo-pontos-jogador1 estado) 
					(jogo-pontos-jogador2 estado) 
					(jogo-jogador estado)))))


;devolve a lista de fios disponiveis	
(defun accoes (jogo)
	(sort (copy-list (mapcar #'(lambda(f)(fio-id f)) (tabuleiro-fios (jogo-tabuleiro jogo)))) #'>))

;; polly, nao sei se e assim, o objectivo e copiar o jogo para outro jogo (novo-jogo como fiz ai), tirar o fio desse novo-jogo e devolver o novo-jogo
(defun resultado (jogo fio)
	(let ((novo-jogo (copia-jogo jogo)))
		(jogo-aplica-jogada! novo-jogo fio)
		novo-jogo))

; devolve a comparacao da lista de fios com uma lista vazia, se a lista estiver vazia o jogo esta terminado
(defun teste-terminal-p (jogo p)
	(declare (ignore p))
	(jogo-terminado-p jogo))

(defun utilidade (jogo i)
	(if (= i 1)
		(- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))
		(- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo))))







;;;;;;;;;;;;;
;;;MINIMAX;;;
;;;;;;;;;;;;;




;;
;;minimax simples
;;

	(defun minimax(prob j_max)
		(values-list (compara_sucessor_func	
				'> 
				prob 
				(problema-estado-inicial prob) 
				(funcall (problema-accoes prob) (problema-estado-inicial prob)) 
				j_max)))	

	;funcao que visita o estado e retorna o valor minmax deste no 
	(defun decide_jogada(prob estado accao j_max)  
		(let* ((best_play (list -1 0 0))
		      (nov_estado (funcall (problema-resultado prob) estado accao))
		      (jogador (funcall (problema-jogador prob) nov_estado)));val_play ->(pontos n_nos_folha)
				(cond	((funcall (problema-teste-corte-p prob) nov_estado 1) 
					;;actualiza o n_nos_folha e o valor do n_nos_folha
						(incf (third best_play))
						(setf (second best_play) 
						      (funcall (problema-funcao-avaliacao prob) nov_estado j_max))
						(setf (first best_play) accao))
					((= jogador j_max) 
						;; o no visitado e um no max
						( setf best_play (compara_sucessor_func '> prob nov_estado (funcall (problema-accoes prob) nov_estado) j_max)) ;invoca o max_func e actualiza os n_nos_folha e o valor minimax do no
						(setf (first best_play) accao))
					((not (= jogador j_max))
						;;o no visitado e um no min
						(setf best_play (compara_sucessor_func '< prob nov_estado (funcall (problema-accoes prob) nov_estado) j_max))
						(setf (first best_play) accao))) ;invoca o min_func e actualiza os n_nos_folha e o valor minimax do no
			best_play)) ;devolve um par com o valor minimax do no e os numero de nos folha visitados

		
		


	;;funcao que visita um estado max e retorna o valor deste no
	(defun compara_sucessor_func (comparador prob estado actions j_max)
		(let((best_play (list -1 0 0)))
			(dolist (action actions)
				(let(	(sucessor_value 0) ;decide_value valor temporario que armazena o par retornado pelo decide-jogada ou pela max_func
					;(nov_estado (funcall (problema-resultado prob) estado action)) ;estado resultante da  primeira accao da lista actions
					 ;numero de nos folha visitados a partir do estado max em estudo.
					)
						;;visita o no
						(setf sucessor_value (decide_jogada prob estado action j_max)) ;visita nos sucessores
						(when (or (= (first best_play) -1) (funcall comparador (second sucessor_value) (second best_play))) 
							;; o valor minimax do nov_estado e maior que o maximo dos valores dos estados ateriormente testados 
							(setf (second best_play) (second sucessor_value))
							(setf (first best_play) action)) ;actualiza valor minimax do no
						(setf (third best_play) (+ (third best_play) (third sucessor_value))))) ;actualiza o numero de nos folha visitados
							   ;;nov_estado testado, falta testar o resto das accoes 
			best_play)) ;retorna o valor do no e o numero de nos folha visitados

	(defun jogador-minimax-simples (jogo jogador max_tempo)
		(declare(ignore max_tempo))
		(first (multiple-value-list (minimax (make-problema	:estado-inicial (copia-jogo jogo) 
									:jogador #'(lambda(estado) (jogo-jogador estado)) 
									:accoes #'(lambda(estado) (accoes estado))
									:resultado #'(lambda(estado accao) (resultado estado accao))
									:teste-corte-p #'(lambda(estado i) (teste-terminal-p estado i)) 
									:funcao-avaliacao #'(lambda(estado i) (utilidade estado i) ) 
									:historico-accoes  #'(lambda(estado) (jogo-historico-jogadas estado)) 
									:chave-equivalencia #'(lambda(estado) 
												(list	(jogo-tabuleiro estado) 
													(jogo-pontos-jogador1 estado) 
													(jogo-pontos-jogador2 estado) 
													(jogo-jogador estado)))) jogador))))

	


;;
;;minimax com cortes alfa beta
;;

	(defun minimax-alfa-beta(prob j_max)
		(values-list (compara_sucessor_func_alfa_beta	
				'> 
				prob 
				(problema-estado-inicial prob) 
				(funcall (problema-accoes prob) (problema-estado-inicial prob))
				(cons most-negative-fixnum most-positive-fixnum) ;->car e alfa e cdr beta  
				0
				j_max)))	

	;funcao que visita o estado e retorna o valor minmax deste no 
	(defun decide_jogada_alfa_beta(prob estado accao alfa_beta depth j_max)  
		(let* ((best_play (list -1 0 0))
		      (nov_estado (funcall (problema-resultado prob) estado accao))
		      (jogador (funcall (problema-jogador prob) nov_estado))
		;	(explore NIL)
			);val_play ->(pontos n_nos_folha)
				;(if timeout
				;	(setf explore NIL)
				;	(setf explore T))
				(cond	((funcall (problema-teste-corte-p prob) nov_estado depth) 
					;;actualiza o n_nos_folha e o valor do n_nos_folha
						(incf (third best_play))
						(setf (second best_play) 
						      (funcall (problema-funcao-avaliacao prob) nov_estado j_max))
						(setf (first best_play) accao))
					((= jogador j_max) 
						;; o no visitado e um no max
						( setf best_play (compara_sucessor_func_alfa_beta '> prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta (+ 1 depth) j_max)) ;invoca o max_func e actualiza os n_nos_folha e o valor minimax do no
						(setf (first best_play) accao))
					((not (= jogador j_max))
						;;o no visitado e um no min
						(setf best_play (compara_sucessor_func_alfa_beta '< prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta (+ 1 depth) j_max))
						(setf (first best_play) accao))) ;invoca o min_func e actualiza os n_nos_folha e o valor minimax do no
			best_play)) ;devolve um par com o valor minimax do no e os numero de nos folha visitados

		
	;;funcao que visita um estado max e retorna o valor deste no
	(defun compara_sucessor_func_alfa_beta (comparador prob estado actions alfa_beta_pai depth j_max)
		(let(	(best_play (list -1 0 0))
			(alfa_beta (cons (car alfa_beta_pai) (cdr alfa_beta_pai))))
				(dolist (action actions)
					(let(	(sucessor_value 0) ;decide_value valor temporario que armazena o par retornado pelo decide-jogada ou pela max_func
						;(nov_estado (funcall (problema-resultado prob) estado action)) ;estado resultante da  primeira accao da lista actions
						 ;numero de nos folha visitados a partir do estado max em estudo.
						)
							;;visita o no
							(setf sucessor_value (decide_jogada_alfa_beta prob estado action alfa_beta depth j_max)) ;visita nos sucessores
							(when (or (= (first best_play) -1) (funcall comparador (second sucessor_value) (second best_play))) 
								;; o valor minimax do nov_estado e maior que o maximo dos valores dos estados ateriormente testados 
								(setf (second best_play) (second sucessor_value))
								(setf (first best_play) action)
								(if (equal comparador '>) 
									(setf (car alfa_beta) (max (car alfa_beta) (second best_play)))
	  								(setf (cdr alfa_beta) (min (cdr alfa_beta) (second best_play))))) ;actualiza valor minimax do no
							(setf (third best_play) (+ (third best_play) (third sucessor_value)))
							(when (>= (car alfa_beta) (cdr alfa_beta)) (return)))) ;actualiza o numero de nos folha visitados
								   ;;nov_estado testado, falta testar o resto das accoes 
				best_play)) ;retorna o valor do no e o numero de nos folha visitados  	


;;
;;minimax com cutoffs VONE  && VTWO
;;



;;;Este jogador corre varias vezes o best-minimax. 
;;;Por cada best-minimax executado, guarda a jogada escolhida e faz uma estimativa da velocidade de processamento de cada no.
;;;Baseado nessa velocidade de processamento e no tempo que resta de execucao, altera a max_depth.
;;;Quando o o tempo estiver quase a acabar retorna a ultima jogada recebida do best-minimax.
	(defun jogador-minimax-v1 (jogo jogador max_tempo)

		(let (	(max_depth 1) 
			(returned_play T) ;ultima jogada retornada pelo best-minimax, caso haja um timeout dentro do minimax, tera o valor NIL
			(best_play NIL)   ;jogada que vai ser retornada pelo jogador
			(tempo_inicial (get-internal-real-time)) ;tempo de inicio de execucao
			(time_start_func 0) ;tempo de processamento do best-minimax
			(time_elapsed 0) ;tempo que passou desde o inicio do jogador-minimax-vbest
			(n_fios (tabuleiro-fios (jogo-tabuleiro jogo)))
			(new_max_depth 0)) ;valor que possivelmente vai ser usado na profundidade do proximo best-minimax


			(loop while returned_play do ;enquanto nao houver timeout ou a profundidade nao for maior que o numero de fios
				(let ((prob	(make-problema	:estado-inicial (copia-jogo jogo) 
								:jogador #'(lambda(estado) (jogo-jogador estado)) 
								:accoes #'(lambda(estado) (accoes estado))
								:resultado #'(lambda(estado accao) (resultado estado accao))
								:teste-corte-p #'(lambda (jogo p) 
											(or (> p max_depth) (jogo-terminado-p jogo))) 
								:funcao-avaliacao #'(lambda(estado i) (+ (heuristica1 estado i)
													 (utilidade estado i) )
													)
								:historico-accoes  #'(lambda(estado) (jogo-historico-jogadas estado)) 
								:chave-equivalencia #'(lambda(estado) 
											(list	(jogo-tabuleiro estado) 
												(jogo-pontos-jogador1 estado) 
												(jogo-pontos-jogador2 estado) 
												(jogo-jogador estado))))))


					(setf time_start_func (get-internal-real-time))
					;(print max_depth)
					(setf returned_play (multiple-value-list (best-minimax 
								
									prob
									jogo
									(cons most-negative-fixnum most-positive-fixnum) ;alfa beta
									max_depth
									jogador ;jogador max
									tempo_inicial ;inicio da funcao jogador-minimax-vbest
									(* max_tempo .9) ;passa lhe .99 do tempo maximo, para garantir que sai do minimax com uma margem de seguranca.
						)))
					(if (first returned_play) ;se nao excedeu o tempo no minimax vai guardar a jogada retornada pelo minimax.
						(progn
							    (setf best_play (list (first returned_play) (second returned_play) (third returned_play)))
							    (if(>= max_depth (length (funcall (problema-accoes prob) jogo))) ;caso tenha conseguido precorrer o minimax ate ao fim, quebra o ciclo de execucao do minimax.
								(setf returned_play NIL)))
						(setf returned_play NIL))
					(setf time_start_func  (/ (- (get-internal-real-time) time_start_func) internal-time-units-per-second)) ;tempo de excucao do best-minimax
					(setf time_elapsed (/ (- (get-internal-real-time) tempo_inicial) internal-time-units-per-second)) ;tempo decorrido desde o inicio do jogador-minimax-vbest.
					;(setf new_max_depth (truncate(* max_depth (/ (- max_tempo time_elapsed) time_start_func))))
					(setf new_max_depth (reverse_factorial (* (/ 4 3)(* (factorial max_depth (length n_fios)) (/ (- (* max_tempo .9) time_elapsed) time_start_func))) (length n_fios)))
					;(progn (print (list 'time_elapsed (float time_elapsed)))(print (list 'time_start_func (float time_start_func))) (print new_max_depth)(print max_depth)) 
					(if(> new_max_depth max_depth) ;escolhe qual a maior profundidade, se for a anterior incrementa-a. 
						(setf max_depth new_max_depth)
						(incf max_depth))
				))
			(if best_play (first best_play)))) ;retorna o id do fio a cortar.

	(defun jogador-minimax-v2 (jogo jogador max_tempo)

		(let (	(max_depth 1) 
			(returned_play T) ;ultima jogada retornada pelo best-minimax, caso haja um timeout dentro do minimax, tera o valor NIL
			(best_play NIL)   ;jogada que vai ser retornada pelo jogador
			(tempo_inicial (get-internal-real-time)) ;tempo de inicio de execucao
			(time_start_func 0) ;tempo de processamento do best-minimax
			(time_elapsed 0) ;tempo que passou desde o inicio do jogador-minimax-vbest
			(n_fios (tabuleiro-fios (jogo-tabuleiro jogo)))
			(new_max_depth 0)) ;valor que possivelmente vai ser usado na profundidade do proximo best-minimax


			(loop while returned_play do ;enquanto nao houver timeout ou a profundidade nao for maior que o numero de fios
				(let ((prob	(make-problema	:estado-inicial (copia-jogo jogo) 
								:jogador #'(lambda(estado) (jogo-jogador estado)) 
								:accoes #'(lambda(estado) (accoes estado))
								:resultado #'(lambda(estado accao) (resultado estado accao))
								:teste-corte-p #'(lambda (jogo p) 
											(or (> p max_depth) (jogo-terminado-p jogo))) 
								:funcao-avaliacao #'(lambda(estado i) (+ (
								heuristica2 estado i #'procura_lagarta #'procura_figura #'calcula_score)
													 (utilidade estado i) )
													)
								:historico-accoes  #'(lambda(estado) (jogo-historico-jogadas estado)) 
								:chave-equivalencia #'(lambda(estado) 
											(list	(jogo-tabuleiro estado) 
												(jogo-pontos-jogador1 estado) 
												(jogo-pontos-jogador2 estado) 
												(jogo-jogador estado))))))


					(setf time_start_func (get-internal-real-time))
					;(print max_depth)
					(setf returned_play (multiple-value-list (best-minimax 
								
									prob
									jogo
									(cons most-negative-fixnum most-positive-fixnum) ;alfa beta
									max_depth
									jogador ;jogador max
									tempo_inicial ;inicio da funcao jogador-minimax-vbest
									(* max_tempo .9) ;passa lhe .99 do tempo maximo, para garantir que sai do minimax com uma margem de seguranca.
						)))
					(if (first returned_play) ;se nao excedeu o tempo no minimax vai guardar a jogada retornada pelo minimax.
						(progn
							    (setf best_play (list (first returned_play) (second returned_play) (third returned_play)))
							    (if(>= max_depth (length (funcall (problema-accoes prob) jogo))) ;caso tenha conseguido precorrer o minimax ate ao fim, quebra o ciclo de execucao do minimax.
								(setf returned_play NIL)))
						(setf returned_play NIL))
					(setf time_start_func  (/ (- (get-internal-real-time) time_start_func) internal-time-units-per-second)) ;tempo de excucao do best-minimax
					(setf time_elapsed (/ (- (get-internal-real-time) tempo_inicial) internal-time-units-per-second)) ;tempo decorrido desde o inicio do jogador-minimax-vbest.
					;(setf new_max_depth (truncate(* max_depth (/ (- max_tempo time_elapsed) time_start_func))))
					(setf new_max_depth (reverse_factorial (* (/ 4 3)(* (factorial max_depth (length n_fios)) (/ (- (* max_tempo .9) time_elapsed) time_start_func))) (length n_fios)))
					;(progn (print (list 'time_elapsed (float time_elapsed)))(print (list 'time_start_func (float time_start_func))) (print new_max_depth)(print max_depth)) 
					(if(> new_max_depth max_depth) ;escolhe qual a maior profundidade, se for a anterior incrementa-a. 
						(setf max_depth new_max_depth)
						(incf max_depth))
				))
			(if best_play (first best_play)))) ;retorna o id do fio a cortar.
			
;minimax com cortes alfa-beta e com decisoes em tempo limitado.
	(defun best-minimax(prob estado alfa_beta max_depth j_max tempo_inicio max_tempo)
		(let((best_play (best_compara_sucessor_func	;sendo o no inicial um no max, procura o valor os nos sucessores e retorna o melhor. 
					(if (= (funcall (problema-jogador prob) estado) j_max)'> '<)				
					prob 
					estado 
					(funcall (problema-accoes prob) estado)
					alfa_beta; ;->car e alfa e cdr beta  
					0
					max_depth
					j_max
					tempo_inicio
					max_tempo)))
			;(print best_play)
			(if best_play (values-list best_play) NIL))) ;caso nao tenha excedido o tempo retorna a melhor jogada, caso contrario retorna o NIL.	

	;funcao que visita o estado, descobre se e um no min ou max e calcula o seu valor minimax. 
	(defun best_decide_jogada(prob estado accao alfa_beta depth max_depth j_max tempo_inicio max_tempo)  
		(let* ((best_play (list -1 0 0)) ;melhor jogada deste no, qual o seu valor e quantos nos foram visitados para esta decisao.
		      (nov_estado (funcall (problema-resultado prob) estado accao)) ;estado resultante da accao.
		      (jogador (funcall (problema-jogador prob) nov_estado)) ;jogador e o min ou max.
			(tempo (/ (- (get-internal-real-time) tempo_inicio) internal-time-units-per-second)) ;tempo decorrido desde o inicio do minimax-vbest
			)
				(if (<= tempo max_tempo) ;timeout?
					;;se nao houve calcula o valor  	
					(cond	((funcall (problema-teste-corte-p prob) nov_estado depth) 
						;;houve corte de profundidade ou o no e um no folha.
						;;actualiza o nos_folha e o valor do n_nos_folha
							(incf (third best_play))
							(setf (second best_play) 
							      (funcall (problema-funcao-avaliacao prob) nov_estado j_max))
							(setf (first best_play) accao)						
							)
						((= jogador j_max)
							;; o no visitado e um no max
							( setf best_play (best_compara_sucessor_func '> prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta depth max_depth j_max tempo_inicio max_tempo)) ;descobre o valor
							(if best_play (setf (first best_play) accao)(setf best_play NIL))) ;caso n tenha havido timeout actualiza o best_play, senao atribui NIL.
						((not (= jogador j_max))
							;;o no visitado e um no min
							(setf best_play (best_compara_sucessor_func '< prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta depth max_depth j_max tempo_inicio max_tempo)) ;descobre o valor
							(if best_play (setf (first best_play) accao)(setf best_play NIL)))) ;caso n tenha havido timeout actualiza o best_play, senao atribui NIL.
					(setf best_play NIL)) ;retorna NIL por ter havido timeout.
				best_play)) ;retorna a best_play

	
	;;funcao que obtem o valor do estado passado como argumento.
	(defun best_compara_sucessor_func (comparador prob estado actions alfa_beta_pai depth max_depth j_max tempo_inicio max_tempo)
		(let(	(best_play (list -1 0 0)) ;ainda nao foi avaliado nenhum no sucessor, inicializado com uma jogada invalida.
			(alfa_beta (cons (car alfa_beta_pai) (cdr alfa_beta_pai)))) ;valores alfa-beta do no actual 
				(dolist (action actions)
					(let(	(sucessor_value 0) ;valor auxiliar que armazena a lista retornada pela funcao best_decide_jogada
						)
							;;obtem o valor resultante
							(setf sucessor_value (best_decide_jogada prob estado action alfa_beta (+ 1 depth) max_depth j_max tempo_inicio max_tempo))
							(when (null sucessor_value) (setf best_play NIL)(return)) ;caso tenha havido timeout quebra o ciclo e devolve NIL
							(when (or (= (first best_play) -1) (funcall comparador (second sucessor_value) (second best_play))) 
								;; o valor minimax do estado sucessor e melhor que os valores dos estados ateriormente testados. Portanto altera o best_play e actualiza o alfa-beta do estado passado como argumento.
								(setf (second best_play) (second sucessor_value))
								(setf (first best_play) action)
								(if (equal comparador '>) 
									(setf (car alfa_beta) (max (car alfa_beta) (second best_play)))
	  								(setf (cdr alfa_beta) (min (cdr alfa_beta) (second best_play))))) ;actualiza valor minimax do no
							(setf (third best_play) (+ (third best_play) (third sucessor_value)))
							(when (>= (car alfa_beta) (cdr alfa_beta)) (return)))) ;houve corte alfa_beta? Se sim quebra o ciclo
				(if (or (null best_play)(= (first best_play) -1)) NIL best_play))) ;retorna o valor do no ou nil se tiver havido timeout.


;;
;;minimax com cutoffs VBEST
;;



;;;Este jogador corre varias vezes o best-minimax. 
;;;Por cada best-minimax executado, guarda a jogada escolhida e faz uma estimativa da velocidade de processamento de cada no.
;;;Baseado nessa velocidade de processamento e no tempo que resta de execucao, altera a max_depth.
;;;Quando o o tempo estiver quase a acabar retorna a ultima jogada recebida do best-minimax.
	(defun jogador-minimax-vbest (jogo jogador max_tempo)

		(let (	(max_depth 1) 
			(returned_play T) ;ultima jogada retornada pelo best-minimax, caso haja um timeout dentro do minimax, tera o valor NIL
			(best_play NIL)   ;jogada que vai ser retornada pelo jogador
			(tempo_inicial (get-internal-real-time)) ;tempo de inicio de execucao
			(time_start_func 0) ;tempo de processamento do best-minimax
			(time_elapsed 0) ;tempo que passou desde o inicio do jogador-minimax-vbest
			(n_fios (tabuleiro-fios (jogo-tabuleiro jogo)))
			(hash_valor_nos_visitados (make-hash-table :test 'equal))
			(new_max_depth 0)) ;valor que possivelmente vai ser usado na profundidade do proximo best-minimax


			(loop while returned_play do ;enquanto nao houver timeout ou a profundidade nao for maior que o numero de fios
				(let ((prob	(make-problema	:estado-inicial (copia-jogo jogo) 
								:jogador #'(lambda(estado) (jogo-jogador estado)) 
								:accoes #'(lambda(estado) (accoes estado))
								:resultado #'(lambda(estado accao) (resultado estado accao))
								:teste-corte-p #'(lambda (jogo p) 
											(or (> p max_depth) (jogo-terminado-p jogo))) 
								:funcao-avaliacao #'(lambda(estado i) (+ (heuristica2 estado i #'procura_lagarta #'procura_figura #'calcula_score)
													 (utilidade estado i) )
													)
								:historico-accoes  #'(lambda(estado) (jogo-historico-jogadas estado)) 
								:chave-equivalencia #'(lambda(estado) 
											(list	(jogo-tabuleiro estado) 
												(jogo-pontos-jogador1 estado) 
												(jogo-pontos-jogador2 estado) 
												(jogo-jogador estado))))))


					(setf time_start_func (get-internal-real-time))
					(setf returned_play (multiple-value-list (best-minimax-vbest 
								
									prob
									jogo
									(cons most-negative-fixnum most-positive-fixnum) ;alfa beta
									max_depth
									jogador ;jogador max
									tempo_inicial ;inicio da funcao jogador-minimax-vbest
									(* max_tempo .9) ;passa lhe .99 do tempo maximo, para garantir que sai do minimax com uma margem de seguranca.
									hash_valor_nos_visitados
						)))
					(if (first returned_play) ;se nao excedeu o tempo no minimax vai guardar a jogada retornada pelo minimax.
						(progn
							    (setf best_play (list (first returned_play) (second returned_play) (third returned_play)))
							    (if(>= max_depth (length (funcall (problema-accoes prob) jogo))) ;caso tenha conseguido precorrer o minimax ate ao fim, quebra o ciclo de execucao do minimax.
								(setf returned_play NIL)))
						(setf returned_play NIL))
					(setf time_start_func  (/ (- (get-internal-real-time) time_start_func) internal-time-units-per-second)) ;tempo de excucao do best-minimax
					(setf time_elapsed (/ (- (get-internal-real-time) tempo_inicial) internal-time-units-per-second)) ;tempo decorrido desde o inicio do jogador-minimax-vbest.
					;(setf new_max_depth (truncate(* max_depth (/ (- max_tempo time_elapsed) time_start_func))))
					(setf new_max_depth (reverse_factorial (* (/ 4 3)(* (factorial max_depth (length n_fios)) (/ (- (* max_tempo .9) time_elapsed) time_start_func))) (length n_fios)))
					;(progn (print (list 'time_elapsed (float time_elapsed)))(print (list 'time_start_func (float time_start_func))) (print new_max_depth)(print max_depth)) 
					(if(> new_max_depth max_depth) ;escolhe qual a maior profundidade, se for a anterior incrementa-a. 
						(setf max_depth new_max_depth)
						(incf max_depth))
				))
			(if best_play (first best_play)))) ;retorna o id do fio a cortar.

(defun reverse_factorial(num b)
	;(print b)
	(let ((depth 0))	
			(loop do
				(setf num (/ num (- b depth)))
				(incf depth)		 
			while (and (> num 1) (not(= depth b))))depth))


;;;funcao definida meramente para destes do projecto
	(defun test_timeout (n)
		(let ((tempo_inicio (get-internal-real-time)))
			(loop do n while (<= (/ (- (get-internal-real-time) tempo_inicio) internal-time-units-per-second) n) )))
;;;funcao definida meramente para destes do projecto
	(defun factorial (n b)
		(let ((res 1))
			(loop do	
				(setf res (* b res))
				(decf b)
				(decf n)
			while (> n 0))
		res))


;minimax com cortes alfa-beta e com decisoes em tempo limitado.
	(defun best-minimax-vbest(prob estado alfa_beta max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados)
		(let((best_play (best_compara_sucessor_func_vbest	;sendo o no inicial um no max, procura o valor os nos sucessores e retorna o melhor. 
					(if (= (funcall (problema-jogador prob) estado) j_max)'> '<)				
					prob 
					estado 
					(funcall (problema-accoes prob) estado)
					alfa_beta; ;->car e alfa e cdr beta  
					0
					max_depth
					j_max
					tempo_inicio
					max_tempo
					hash_valor_nos_visitados)))
			;(print best_play)
			(if best_play (progn (setf (gethash (funcall (problema-chave-equivalencia prob) estado) hash_valor_nos_visitados) (second best_play))(values-list best_play) ) NIL))) ;caso nao tenha excedido o tempo retorna a melhor jogada, caso contrario retorna o NIL.	

	;funcao que visita o estado, descobre se e um no min ou max e calcula o seu valor minimax. 
	(defun best_decide_jogada_vbest(prob estado accao alfa_beta depth max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados)  
		(let* ((best_play (list -1 0 0)) ;melhor jogada deste no, qual o seu valor e quantos nos foram visitados para esta decisao.
		      (nov_estado (funcall (problema-resultado prob) estado accao)) ;estado resultante da accao.
		      (jogador (funcall (problema-jogador prob) nov_estado)) ;jogador e o min ou max.
			(tempo (/ (- (get-internal-real-time) tempo_inicio) internal-time-units-per-second)) ;tempo decorrido desde o inicio do minimax-vbest
			)
				(if (<= tempo max_tempo) ;timeout?
					;;se nao houve calcula o valor  	
					(cond	((funcall (problema-teste-corte-p prob) nov_estado depth) 
						;;houve corte de profundidade ou o no e um no folha.
						;;actualiza o nos_folha e o valor do n_nos_folha
							(incf (third best_play))
							(setf (second best_play) 
							      (funcall (problema-funcao-avaliacao prob) nov_estado j_max))
							(setf (first best_play) accao)						
							)
						((= jogador j_max)
							;; o no visitado e um no max
							( setf best_play (best_compara_sucessor_func_vbest '> prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta depth max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados)) ;descobre o valor
							(if best_play (setf (first best_play) accao)(setf best_play NIL))) ;caso n tenha havido timeout actualiza o best_play, senao atribui NIL.
						((not (= jogador j_max))
							;;o no visitado e um no min
							(setf best_play (best_compara_sucessor_func_vbest '< prob nov_estado (funcall (problema-accoes prob) nov_estado) alfa_beta depth max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados)) ;descobre o valor
							(if best_play (setf (first best_play) accao)(setf best_play NIL)))) ;caso n tenha havido timeout actualiza o best_play, senao atribui NIL.
					(setf best_play NIL)) ;retorna NIL por ter havido timeout.
				(if best_play (setf (gethash (funcall (problema-chave-equivalencia prob) nov_estado) hash_valor_nos_visitados) (second best_play)))
				best_play)) ;retorna a best_play

	
	;;funcao que obtem o valor do estado passado como argumento.
	(defun best_compara_sucessor_func_vbest (comparador prob estado actions alfa_beta_pai depth max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados)
		(let(	(best_play (list -1 0 0)) ;ainda nao foi avaliado nenhum no sucessor, inicializado com uma jogada invalida.
			(lista_estados_repetidos NIL)
			(profundidade_repetida T)			
			(alfa_beta (cons (car alfa_beta_pai) (cdr alfa_beta_pai)))) ;valores alfa-beta do no actual 
				(setf lista_estados_repetidos (mapcar #'(lambda(action)
										(let*((nov_estado (funcall (problema-resultado prob) estado action))
								    		     (valor_nov_estado (gethash (funcall (problema-chave-equivalencia prob) nov_estado) hash_valor_nos_visitados)))
											(list nov_estado valor_nov_estado action)))
								actions))
				(dolist	(estado_repetido lista_estados_repetidos)
						(if (null (second estado_repetido))
							(setf profundidade_repetida T)))
				(if profundidade_repetida
					(progn	(sort lista_estados_repetidos comparador :key #'(lambda(x) (third x)))
						(setf actions (mapcar #'(lambda(estado_repetido)
										(third estado_repetido))
									lista_estados_repetidos))))	
				(dolist (action actions) ;os nos desta profundidade nunca foram precorridos
					(let(	(sucessor_value 0) ;valor auxiliar que armazena a lista retornada pela funcao best_decide_jogada
						)
							;;obtem o valor resultante
							(setf sucessor_value (best_decide_jogada_vbest prob estado action alfa_beta (+ 1 depth) max_depth j_max tempo_inicio max_tempo hash_valor_nos_visitados))
							(when (null sucessor_value) (setf best_play NIL)(return)) ;caso tenha havido timeout quebra o ciclo e devolve NIL
							(when (or (= (first best_play) -1) (funcall comparador (second sucessor_value) (second best_play))) 
								;; o valor minimax do estado sucessor e melhor que os valores dos estados ateriormente testados. Portanto altera o best_play e actualiza o alfa-beta do estado passado como argumento.
								(setf (second best_play) (second sucessor_value))
								(setf (first best_play) action)
								(if (equal comparador '>) 
									(setf (car alfa_beta) (max (car alfa_beta) (second best_play)))
	  								(setf (cdr alfa_beta) (min (cdr alfa_beta) (second best_play))))) ;actualiza valor minimax do no
							(setf (third best_play) (+ (third best_play) (third sucessor_value)))
							(when (>= (car alfa_beta) (cdr alfa_beta)) (return)))) ;houve corte alfa_beta? Se sim quebra o ciclo
				(if (or (null best_play)(= (first best_play) -1)) NIL best_play))) ;retorna o valor do no ou nil se tiver havido timeout.  	
	





	(defun safe_log(num)
			(if(= num 1) 1 
				(log num)))


;;;
;;;HEURISTICA
;;;

;;;Nesta heuristica e decomposto o tabuleiro em 2 tipos de conjuntos de moedas:
; lagartas -> conjuntos de moedas que comecam com uma moeda de 1 fio ligada a varias de 2 fios.

; figuras -> conhuntos de moedas com mais de 1 fio ligadas entre si. 
;;; Apos a decomposicao do tabuleiro e atribuido a pontucao a cada jogador e feita a diferenca de pontos.

	(defun heuristica2 (jogo j_max lagarta_func procura_fig_func score_func)
		(let(	(pontos_moedas_1fio 0) ;sumatorio de pontos da moedas de 1 fio
			(board (jogo-tabuleiro jogo)) ;tabuleiro do jogo em analise
			(jogador (jogo-jogador jogo)) ;jogador que esta a jogar
			(l_moedas_visitadas ()) ;lista de moedas que foram analisadas
			(l_figuras ()) ;cada figura sera um lista (num_fios,num_moedas,sumatorio de pontos da figura)
			(aval_fig) 
			(res 0) ;resultado da heuristica2
		     )
		;; ciclos que precorrem o tabuleiro a procura de lagartas. Quando encontradas e calculado o valor delas e assinalado que essas moedas ja foram investigadas.
			(dotimes (l (tabuleiro-linhas board))
				(dotimes (c (tabuleiro-colunas board))
					(let*(	(pos (cria-posicao l c))
						(n_fios (length (tabuleiro-fios-posicao board pos))))
							(when	(and (= n_fios 1) (not (member pos l_moedas_visitadas :test #'posicoes-iguais-p)))
									;;lagarta por explorar encontrada	
									(let((lagarta (multiple-value-list ( funcall lagarta_func board pos l_moedas_visitadas))))
										(setf pontos_moedas_1fio (+ pontos_moedas_1fio (first lagarta))) ;soma os pontos ao da lagarta aos pontos de moedas de um fio encontradas
										(setf l_moedas_visitadas (second lagarta)) ;actualiza as moedas visitadas
										
									)))))
		;;ciclos que precorrem o tabuleiro a procura das figuras. Quando encontradas e calculado o valor delas e assinalado que essas moedas ja foram investigadas.
			(dotimes (l (tabuleiro-linhas board))
				(dotimes (c (tabuleiro-colunas board))
					(let*(	(pos (cria-posicao l c))
						(n_fios (length (tabuleiro-fios-posicao board pos))))
							(when	(and (> n_fios 1) (not (member pos l_moedas_visitadas :test #'posicoes-iguais-p)))
									;figura por explorar encontrada
									(let ((fig (multiple-value-list (funcall procura_fig_func 0 board (list pos) () l_moedas_visitadas)))) ;procura a figura e actualiza as moeadas visitadas e adiciona a figura encontrada na lista de figuras
										(when (not (null fig))
											(setf l_moedas_visitadas (append l_moedas_visitadas (second fig)))
											(setf (second fig) (length (second fig))) 
											(setf l_figuras (cons fig l_figuras))))))))
			(if (= jogador j_max) ;calcula o resultado da lagarta no ponto de vista do jogador max 
				(progn	(setf res (+ res pontos_moedas_1fio)) ;da lhe os pontos 
					(setf aval_fig NIL)) ;max e o primeiro a jogar
				(progn	(setf res (- res pontos_moedas_1fio)) ;perde os pontos
					(setf aval_fig T))) ;min e o primeiro a jogar
			(setf l_figuras (sort l_figuras #'< :key #'(lambda(x) (third x)))) ;ordena por ordem crescente
			(setf res (funcall score_func res aval_fig  l_figuras)) ;atribui ao resultado as figuras no ponto de vista do jogador max 
			res))

;;; recebe o resultado e vai distribuido as figura para cada jogador (max e min).
	(defun calcula_score (res aval_fig l_figuras) 
			(dolist (fig l_figuras)
				(if aval_fig ;ve quem e a jogar
					(progn (if (funcall 'evenp (- (/ (first fig) 2) (second fig))) ;sou min, se for par e meu
								(setf res (+ res (third fig)))
								(setf res (- res (third fig)))))
					(progn 	(if (funcall 'oddp (- (/ (first fig) 2) (second fig))) ;sou max, se for impar e meu
								(setf res (+ res (third fig)))
								(setf res (- res (third fig))))))
				(setf aval_fig (not aval_fig)) ;muda o jogador que vai jogar
			)
		res)



;;; DFS em grafo que procura a figura que contem a moeda passada na fronteira.
;;;recebe: 	A fronteira (nos gerados que ainda nao foram explorados)
	;	board->o tabuleiro que esta a explorar
	;	l_moedas_figura->as moedas pertencentes a figura
	;	l_moedas_visitadas-> moedas pertencentes a outras figuras ou lagartas
;;;chama recursivamente a procura explorando os nos presentes na fronteira colocando os na l_moedas_figura
;;;					para cada no gerado caso nao pertenca a fronteira, l_moedas figura nem as l_moedas_visitadas coloca no inicio da fronteira.
;;; qunado nao existem mais nos na fronteira, e retornada a figura (numero de nos gerados, numero de moedas e os pontos presentes na figura).
 
	(defun procura_figura(n_nos_gerados board fronteira l_moedas_figura l_moedas_visitadas)
		(let((no_explorado (car fronteira)) ;retira o no inicial da fronteira para ser explorado
		     (pontos_fig 0))
			(setf fronteira (cdr fronteira)) ;actualiza a fronteira
			(cond ((not(or (member no_explorado l_moedas_figura :test #'posicoes-iguais-p) (member no_explorado fronteira :test #'posicoes-iguais-p) (member no_explorado l_moedas_visitadas :test #'posicoes-iguais-p))) ;caso o no nao pertenca nao tenha sido gerado nem pertenca a outra figura ou lagarta				
				(let((l_sucessores_fios (tabuleiro-fios-posicao board no_explorado)) ;lista de fios que estao ligados ao no_explorado
				     (no_sucessor))
					(dolist(sucessor l_sucessores_fios) ;gera sucessores do no
						(if(posicoes-iguais-p (fio-origem sucessor) no_explorado) ;escolhe o outro no que esta ligado ao fio do no_sucessor.
							(setf no_sucessor (fio-destino sucessor))
							(setf no_sucessor (fio-origem sucessor)))
						(when (not (member no_sucessor l_moedas_visitadas :test #'posicoes-iguais-p)) ;se nao pertencer a outra figura
							(when (not(or (member no_sucessor l_moedas_figura :test #'posicoes-iguais-p) (member no_sucessor fronteira :test #'posicoes-iguais-p))) ;se nao tiver sido gerada ou explorada
								(setf fronteira (cons no_sucessor fronteira))) ;adiciona a fronteira
							(incf n_nos_gerados))) ;aumenta o numero de nos gerados.
				)
				(setf pontos_fig (tabuleiro-moeda-posicao board no_explorado)) ;calcula o valor do no
				(setf l_moedas_figura (cons no_explorado l_moedas_figura))) ;adiciona a lista de moedas pertencente a figura.
			      (T 0)
			)
			(if (not (equal (car fronteira) NIL)) ;se ainda houver nos para ser explorados chama outra vez a procura_fig com os valores actuais dos argumentos. No final actualiza o numero de nos gerados, a lista de moedas pertencentes a figura e o numero de pontos
				(let ((returned_val (multiple-value-list (procura_figura n_nos_gerados board fronteira l_moedas_figura l_moedas_visitadas))))
					(setf n_nos_gerados (first returned_val))
					(setf l_moedas_figura (second returned_val))
					(setf pontos_fig (+ pontos_fig (third returned_val)))))
			(values-list (list n_nos_gerados l_moedas_figura pontos_fig)))) ;retorna a figura.

;;;recebe uma moeda de 1 fio
;; percorre a lagarta ate chegara a moeda final (1 fio ou mais 2 fios) 
	(defun procura_lagarta (board pos l_moedas_visitadas)
		(let ((fios_posicao (tabuleiro-fios-posicao board pos))
		     (res 0) ;pontos da lagarta
		     (fio_anterior NIL)
		     (pos_sucessor))		
			(loop do ;ciclo que procura a moeda ligada a pos, que ainda nao foi explorada
				(setf res (+ res (tabuleiro-moeda-posicao board pos)))	;soma o valor da moeda que esta a ser explorada aos pontos da lagarta				
				(if(equal (fio-origem (first fios_posicao)) pos) ;descobre a moeda ligada ao fio da moeda pos
					(setf pos_sucessor (fio-destino (first fios_posicao)))
					(setf pos_sucessor (fio-origem (first fios_posicao))))
					
				(when (not (member pos_sucessor l_moedas_visitadas :test #'equal)) ;se a moeda explorada nao foi visitada
					(setf l_moedas_visitadas (cons pos l_moedas_visitadas)) ;adiciona a lista de moedas exploradas
					(when (not (null pos_sucessor)) ;se foi encontrada uma moeda para ser explorada
						(setf fios_posicao (tabuleiro-fios-posicao board pos_sucessor))) ;actualiza os fios a ser explorados
					(if (or (equal (fio-origem (first fios_posicao)) pos) (equal pos (fio-destino (first fios_posicao)))) ;guarda o fio que foi anteriormente explorado
						(setf fio_anterior (first fios_posicao))
						(setf fio_anterior (second fios_posicao)))
					(setf pos pos_sucessor) ;actualiza a posicao a ser explorada
					(setf fios_posicao (remove fio_anterior fios_posicao :test #'equal))	;remove o fio que ja foi explorado na lista de fios para ser explorados.	
				)
			while (and(posicao-p pos)(< (length fios_posicao) 2))) ;se ainda houver posicoes por explorar continua o ciclo
			(values-list (list res l_moedas_visitadas)))) ;retorna o sumatorio dos pontos e o numero de moedas visitadas


;;;heuristica que da establece por odem decrescente prioridade as moedas com so um fio ligado, depois as moedas com 2 fios ligados e por ultimo as de 3 ou mais fios ligados.  
	(defun heuristica1 (jogo j_max)
		(let(	(n_moeda_1fio 0)
			(n_moeda_maior_2_fios 0)
			(n_moeda_2_fios 0)
			(board (jogo-tabuleiro jogo))
			(jogador (jogo-jogador jogo))
			)
				(dotimes (l (tabuleiro-linhas board))
					(dotimes (c (tabuleiro-colunas board))
						(let*(	(pos (cria-posicao l c))
							(n_fios (length (tabuleiro-fios-posicao board pos))))
								(cond	((= n_fios 1)	(incf n_moeda_1fio))
									((= n_fios 2)	(incf n_moeda_2_fios))
									((> n_fios 2)(incf n_moeda_maior_2_fios))
									(T 0)))))
				(if (= jogador j_max) (progn (setf n_moeda_1fio 0) (setf n_moeda_maior_2_fios 0)) (setf n_moeda_2_fios 0))				
				(* jogador (+	(* (/ 1 5) n_moeda_1fio)
						(* (/ 1 5) n_moeda_2_fios)
						(* (/ 1 8) n_moeda_maior_2_fios)))))
;;;;;;;;;;;;;;;;;;;;;;
;;;;Material Apoio;;;;
;;;;;;;;;;;;;;;;;;;;;;


	;(load (compile-file "interface-moedas.lisp"))
	;(load (compile-file "exemplos.lisp"))
	(load "interface-moedas.fas")  
	(load "exemplos.fas")
