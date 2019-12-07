(define-module (nomunofu colorizer))

(import (scheme assume))

;; TODO: replace with scheme show

(define %colors
  `((CLEAR       .   "0")
    (RESET       .   "0")
    (BOLD        .   "1")
    (DARK        .   "2")
    (UNDERLINE   .   "4")
    (UNDERSCORE  .   "4")
    (BLINK       .   "5")
    (REVERSE     .   "6")
    (CONCEALED   .   "8")
    (BLACK       .  "30")
    (RED         .  "31")
    (GREEN       .  "32")
    (YELLOW      .  "33")
    (BLUE        .  "34")
    (MAGENTA     .  "35")
    (CYAN        .  "36")
    (WHITE       .  "37")
    (ON-BLACK    .  "40")
    (ON-RED      .  "41")
    (ON-GREEN    .  "42")
    (ON-YELLOW   .  "43")
    (ON-BLUE     .  "44")
    (ON-MAGENTA  .  "45")
    (ON-CYAN     .  "46")
    (ON-WHITE    .  "47")))

(define (escape color)
  (assume color)
  (string-append "\x1b[" color "m"))

(define-public (colorizer color str)
  (string-append (escape (assq-ref %colors color)) str (escape (assq-ref %colors 'CLEAR))))
