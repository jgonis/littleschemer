#lang scheme
;;An initial attempt at cycle detection for a work project

(define cycle-detection
  (lambda (graph)
    (quote ())))

(define get-equal-nodes
  (lambda (tortoise hare graph)
    (cond ((or (get-next-node tortoise graph) (get-next-node hare graph))
           #f)
           ((eq? tortoise hare) tortoise)
          (else (get-equal-nodes (get-next-node tortoise graph) 
                                 (get-next-node (get-next-node hare graph) graph) 
                                 graph)))))

(define find-first-repetition
  (lambda (tortoise hare mu graph)
    (cond ((or (get-next-node tortoise graph) (get-next-node hare graph))
           #f)
          ((eq? tortoise hare) mu)
          (else (find-first-repetition (get-next-node tortoise graph) 
                                      (get-next-node hare graph) (+ mu 1) graph)))))

(define get-next-node
  (lambda (atom graph)
    (cond ((null? graph) #f)
          ((eq? (get-node graph) atom) (get-link graph))
          (else (get-next-node atom (cdr graph))))))

(define get-node
  (lambda (graph)
    (car (car graph))))

(define get-link
  (lambda (graph)
    (car (cdr (car graph))))) 