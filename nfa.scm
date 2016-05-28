;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Project Scope: Non-deterministic Computing 
; CSC 33500 Programming Language Paradigms
; Spring 2016
; Weifan Lin & Aaron Bridgemohan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; member?. This function checks if an atom is in a list.
(define (member? a li)
  (cond
    ((null? li) #f)
    (else (or (equal? a (car li))
              (member? a (cdr li))))
    )
  )


; sub-machine. This function takes a state and a machine, returns a list of the transitions from that state.
(define (sub-machine state Machine)
  (cond
    ((null? Machine) '())
    ((member? state (car Machine)) (cdar Machine))
    (else (sub-machine state (cdr Machine)))
    )
  )


; next-states. This function takes a symbol and a machine (sub-machine), returns a list of possible destination states
; given the symbol.
(define (next-states symbol Machine)
  (cond
    ((null? Machine) '())
    ((equal? symbol (caar Machine)) (cdr (car Machine)))
    (else (next-states symbol (cdr Machine))) 
    )
)


; transitions. c
(define (transitions state symbol Machine)
  (cond
    ((null? Machine) '())
    (else
     (next-states symbol (sub-machine state (cdr Machine))))
    )
  )



; end-state. This function returns an accepting state of a machine.
(define end-state
  (lambda (machine)
    (car machine)))



; backtracking. This function is the most important procedure in our program, as it tells the program on which moves are
; valid and invalid that require backtracking. The following function does the backtracking for our program. The main
; purpose of this function is to run through every possible path for a given input string, after the last character of
; the string has been read and if the current state is an accepting state, then the singleton list of that current state
; will be returned. 
(define (backtracking string start next-states eps-states Machine)
  (cond
    ((and (null? string) (not (atom-member? start (end-state Machine)))) '()) ; string is null & start != end => empty list
    ((and (null? string) (atom-member? start (end-state Machine))) (list start)) ; string is null & start = end => end
    ((and (null? next-states) (null? eps-states)) '())  ; next-states and eps-states are null => empty list
    ((null? next-states) ; next-states is null, check eps-states
     
     (if (null? (nfa-execute string (car eps-states) Machine))
         (backtracking string start next-states (cdr eps-states) Machine)
         (cons start (nfa-execute string (car eps-states) Machine)))) 

    (else ; eps-states is null, check next-states
     (if (null? (nfa-execute (cdr string) (car next-states) Machine))
         (backtracking string start (cdr next-states) eps-states Machine)
         (cons start (nfa-execute (cdr string) (car next-states) Machine))))
    )
  )


; nfa-execute. This function takes an input string, a start state, and an NFA, returns the list of transition states
; if the input string can be accepted by the NFA.
(define (nfa-execute string start Machine)
  (cond
    ((null? string)
     (if (not (member? start (end-state Machine)))
         '()
         (list start))) 
    (else (backtracking string start (transitions start (car string) Machine) (transitions start 'eps Machine) Machine))
    )
  )





; Testing
(define m1 '((4) (1 (a 1 2) (b 1) (c 1)) (2 (b 3)) (3 (c 4))))
(define m2 '((3) (0 (a 1) (eps 2)) (1 (b 3)) (2 (a 2 3))))


(nfa-execute '(a b c) 1 m1) ;returns '(1 2 3 4)
(nfa-execute '(b a c b a a b c) 1 m1) ;returns ’(1 1 1 1 1 1 2 3 4)
(nfa-execute '(a b c c) 1 m1) ;returns '()
(nfa-execute '(a a a a a a a a a a a) 0 m2) ;returns ’(0 2 2 2 2 2 2 2 2 2 2 2 3)
(nfa-execute '(a b) 0 m2) ;returns '(0 1 3)
(nfa-execute '(a b a a a b) 0 m2) ;returns '()
(nfa-execute '(a) 0 m2) ;returns ’(0 2 3)
(nfa-execute '() 0 m2) ; returns '()






    



  