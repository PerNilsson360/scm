;; A scheme translation of
;; http://math.andrej.com/2012/11/08/
;;        how-to-implement-dependent-type-theory-i 

;; (** Abstract syntax of expressions. *)
;; type expr =
;;   | Var of variable
;;   | Universe of int
;;   | Pi of abstraction
;;   | Lambda of abstraction
;;   | App of expr * expr
 
;; (** An abstraction [(x,t,e)] indicates
;;;    that [x] of type [t] is bound in [e]. *)
;; and abstraction = variable * expr * expr
