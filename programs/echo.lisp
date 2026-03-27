;; echo — reads lines and echoes them back, until you type "bye"
;;
;; Memory layout:
;;   1024: current char (cell)
;;   1026: loop index   (cell)
;;   1028+: line buffer (bytes)

(const BUF int 1028)
(const I   int 1026)
(const C   int 1024)

;; compare two null-terminated byte strings
;; uses J (1022) as loop counter to avoid clobbering I
(const J int 1022)

(def streq ((a : int) (b : int)) : bool
  (store J 0)
  (while (if (= (deref8 (+ a (deref J))) (deref8 (+ b (deref J))))
           (!= (deref8 (+ a (deref J))) 0)
           (< 1 0))
    (store J (+ (deref J) 1)))
  (= (deref8 (+ a (deref J))) (deref8 (+ b (deref J)))))

(def main () : void
  (while (< 0 1)
    ;; read a line into BUF
    (store I 0)
    (while (do (store C (key))
               (!= (deref C) 10))
      (store8 (+ BUF (deref I)) (deref C))
      (store I (+ (deref I) 1)))
    ;; null-terminate
    (store8 (+ BUF (deref I)) 0)

    ;; if line is "bye", exit
    (if (streq BUF "bye")
      (bye)
      ;; otherwise echo the line back
      (do (store I 0)
          (while (!= (deref8 (+ BUF (deref I))) 0)
            (emit (deref8 (+ BUF (deref I))))
            (store I (+ (deref I) 1)))
          (emit 10)))))
