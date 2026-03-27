;; echo — reads lines and echoes them back, until you type "bye"
;;
;; Memory layout:
;;   1024: current char (cell)
;;   1026: loop index   (cell)
;;   1028+: line buffer (bytes)

(const BUF int 1028)
(const I   int 1026)
(const C   int 1024)

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
    (if (if (= (deref8 BUF) 98)
          (if (= (deref8 (+ BUF 1)) 121)
            (if (= (deref8 (+ BUF 2)) 101)
              (= (deref I) 3)
              (< 1 0))
            (< 1 0))
          (< 1 0))
      (bye)
      ;; otherwise echo the line back
      (do (store I 0)
          (while (!= (deref8 (+ BUF (deref I))) 0)
            (emit (deref8 (+ BUF (deref I))))
            (store I (+ (deref I) 1)))
          (emit 10)))))
