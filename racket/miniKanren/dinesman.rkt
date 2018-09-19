#lang racket/base

;; Dinesman's multiple-dwelling problem
;;
;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors
;; of an apartment house that contains only five floors.
;; - Baker does not live on the top floor.
;; - Cooper does not live on the bottom floor.
;; - Fletcher does not live on either the top or the bottom floor.
;; - Miller lives on a higher floor than does Cooper.
;; - Smith does not live on a floor adjacent to Fletcher's.
;; - Fletcher does not live on a floor adjacent to Cooper's.
;;
;; Where does everyone live?

(require cKanren/miniKanren
         cKanren/neq)


;; Utils

;; list `xs` is a subset of list `ys`.
(define (⊆o xs ys)
  (conde
   [(nullo xs)]
   [(fresh [hd tl]
      (== xs `(,hd . ,tl))
      (membero hd ys)
      (⊆o tl ys))]))

;; (run 1 [q]
;;   ;; (⊆o '(b c) '(a b c d)) ; '(_.0)
;;   ;; (⊆o '() '(a b c d))    ; '(_.0)
;;   ;; (⊆o '(a d) '(a b c d)) ; '(_.0)
;;   ;; (⊆o '(e) '(a b c d))   ; '()
;;   )

;; floor `f1' is above floor `f2' in floors `fs'.
(define (aboveo f1 f2 fs)
  (fresh [hd tl]
    (== fs `(,hd . ,tl))
    (conde
     [(== f2 hd)
      (membero f1 tl)]
     [(!= f1 hd)
      (aboveo f1 f2 tl)])))

;; (run 3 [q]
;;   ;; (aboveo 'b q '(a b c)) ; '(a)
;;   ;; (aboveo 'b 'a q)       ; '((a b . _.0)
;;   ;;                        ;   (a _.0 b . _.1)
;;   ;;                        ;   ((_.0 a b ._.1) : (=/= ((_.0 . b)))))
;;   )

;; floor `f1` is not immediately below the floor `f2` in floors `fs`.
(define (!below-neighboro f1 f2 fs)
    (fresh [hd1 hd2 tl]
      (conde
       ;; f2 is below f1
       [(== fs `(,hd1 . ,tl))
        (== f2 hd1)
        (membero f1 tl)]
       ;; f1 is below, but not directly
       [(== fs `(,hd1 ,hd2 . ,tl))
        (== f1 hd1)
        (!= f2 hd2)
        (membero f2 tl)]
       ;; Right now, I don't now
       [(== fs `(,hd1 . ,tl))
        (!= f1 hd1)
        (!below-neighboro f1 f2 tl)])))

;; (run 3 [q]
;;   ;; (!below-neighboro 'b q '(a b c d))  ; '(a d)
;;   ;; (!below-neighboro 'b 'c '(a b c d)) ; '()
;;   ;; (!below-neighboro 'b 'a '(a b c d)) ; '(_.0)
;;   ;; (!below-neighboro 'a 'b q)          ; '((b a . _.0)
;;                                          ;   (b _.0 a . _.1)
;;                                          ;   ((a _.0 b . _.1) : (=/= ((_.0 . b)))))
;;   )

;; Floors `f1' and `f2' are not adjacent in floors `fs'.
(define (!neighboro f1 f2 fs)
  (conj
    (!below-neighboro f1 f2 fs)
    (!below-neighboro f2 f1 fs)))

;; (run* [q]
;;   ;; (!neighboro 'a 'e '(a b c d e)) ; '(_.0)
;;   ;; (!neighboro 'a q '(a b c d e))  ; '(c d e)
;;   ;; (!neighboro 'c q '(a b c d e))  ; '(a e)
;;   )


;; Dinesman's multiple-dwelling problem

(run1 [q]
  (fresh [first second third fourth fifth]
    ;; Baker, Cooper, Fletcher, Miller, and Smith live on different
    ;; floors
    (== `(,first ,second ,third ,fourth ,fifth) q)
    (⊆o '(Backer Cooper Fletcher Miller Smith) q)
    ;; - Baker does not live on the top floor.
    (!= fifth 'Backer)
    ;; - Cooper does not live on the bottom floor.
    (!= first 'Cooper)
    ;; - Fletcher does not live on either the top or the bottom floor.
    (!= fifth 'Fletcher) (!= first 'Fletcher)
    ;; - Miller lives on a higher floor than does Cooper.
    (aboveo 'Miller 'Cooper q)
    ;; - Smith does not live on a floor adjacent to Fletcher's.
    (!neighboro 'Smith 'Fletcher q)
    ;; - Fletcher does not live on a floor adjacent to Cooper's.
    (!neighboro 'Fletcher 'Cooper q)
    ))

;; Result: '((Smith Cooper Backer Fletcher Miller))
