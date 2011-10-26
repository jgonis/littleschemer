(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\simply.scm")
(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\database.scm")
(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\functions.scm")
(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\pronounce.scm")
(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\spread.scm")
(load "d:\\jeffs Folder\\Development\\scheme\\simply scheme\\match.scm")

(define beatle?
  (lambda (person)
	(member? person '(john paul george ringo))))

(define (acronym phrase)