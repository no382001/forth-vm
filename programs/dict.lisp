;; dict.lisp — Forth dictionary and simple REPL
;;
;; A linked-list dictionary mapping word names to execution tokens.
;; Demonstrates: heap allocation, linked list traversal, string matching.
;;
;; Dictionary entry layout (6 bytes):
;;   +0: link (cell) — previous entry, 0 = end
;;   +2: name (cell) — pointer to null-terminated string
;;   +4: xt   (cell) — execution token (integer tag)
;;
;; Memory map:
;;   4096-4300: compiler variable slots
;;   5000-5019: scratch cells
;;   5050-5059: dictionary state
;;   6000-6255: word input buffer
;;   8000+:     dictionary heap

;; ---- scratch cells ----
(const T0 int 5000)   ; print_str index
(const T1 int 5002)   ; dict_find: entry walker
(const T2 int 5004)   ; dict_find: result
(const T3 int 5006)   ; read_line: index
(const T4 int 5008)   ; streq: counter
(const T5 int 5010)   ; dict_words: entry walker
(const CH int 5012)   ; current input char

;; ---- dictionary state ----
(const LATEST int 5050)
(const HERE   int 5052)

;; ---- I/O ----
(const BUF int 6000)

;; ---- heap ----
(const HEAP int 8000)

;; ============================================================
;; string utilities
;; ============================================================

;; compare two null-terminated byte strings
;; uses: T4
(def streq ((a : int) (b : int)) : bool
  (store T4 0)
  (while (if (= (deref8 (+ a (deref T4))) (deref8 (+ b (deref T4))))
           (!= (deref8 (+ a (deref T4))) 0)
           (< 1 0))
    (store T4 (+ (deref T4) 1)))
  (= (deref8 (+ a (deref T4))) (deref8 (+ b (deref T4)))))

;; print null-terminated string
;; uses: T0
(def print_str ((s : int)) : void
  (store T0 0)
  (while (!= (deref8 (+ s (deref T0))) 0)
    (emit (deref8 (+ s (deref T0))))
    (store T0 (+ (deref T0) 1))))

;; ============================================================
;; dictionary
;; ============================================================

(def dict_init () : void
  (store LATEST 0)
  (store HERE HEAP))

;; add entry: link=LATEST, name ptr, xt; update LATEST and HERE
(def dict_add ((name : int) (xt : int)) : void
  (let ((h (deref HERE)))
    (store h (deref LATEST))
    (store (+ h 2) name)
    (store (+ h 4) xt)
    (store LATEST h)
    (store HERE (+ h 6))))

;; find word in dictionary, return xt or 0
;; uses: T1 (walker), T2 (result)
(def dict_find ((name : int)) : int
  (store T1 (deref LATEST))
  (store T2 0)
  (while (if (!= (deref T1) 0) (= (deref T2) 0) (< 1 0))
    (if (streq name (deref (+ (deref T1) 2)))
      (store T2 (deref (+ (deref T1) 4)))
      (store T1 (deref (deref T1)))))
  (deref T2))

;; print all words in dictionary
;; uses: T5
(def dict_words () : void
  (store T5 (deref LATEST))
  (while (!= (deref T5) 0)
    (print_str (deref (+ (deref T5) 2)))
    (emit 32)
    (store T5 (deref (deref T5))))
  (emit 10))

;; ============================================================
;; input
;; ============================================================

;; read a line into BUF, null-terminate, return length (-1 on EOF)
;; uses: T3, CH
(def read_line () : int
  (store T3 0)
  (while (do (store CH (key))
             (if (= (deref CH) -1) (< 1 0)
               (!= (deref CH) 10)))
    (store8 (+ BUF (deref T3)) (deref CH))
    (store T3 (+ (deref T3) 1)))
  (store8 (+ BUF (deref T3)) 0)
  (if (if (= (deref CH) -1) (= (deref T3) 0) (< 1 0))
    -1
    (deref T3)))

;; ============================================================
;; dispatch — execute a word by its xt
;; ============================================================

;; xt assignments:
;;   1 = bye      — exit
;;   2 = hello    — print greeting
;;   3 = words    — list dictionary

(def dispatch ((xt : int)) : void
  (if (= xt 1) (bye)
    (if (= xt 2) (do (print_str "Hello!") (emit 10))
      (if (= xt 3) (dict_words)
        (store T0 0)))))

;; ============================================================
;; main
;; ============================================================

(def main () : void
  (dict_init)

  ;; populate dictionary
  (dict_add "bye" 1)
  (dict_add "hello" 2)
  (dict_add "words" 3)

  ;; REPL loop
  (while (< 0 1)
    (let ((len (read_line)))
      (if (= len -1) (bye)
        (if (!= (deref8 BUF) 0)
          (let ((xt (dict_find BUF)))
            (if (= xt 0)
              (do (print_str BUF) (print_str " ?") (emit 10))
              (dispatch xt)))
          (store T0 0))))))
