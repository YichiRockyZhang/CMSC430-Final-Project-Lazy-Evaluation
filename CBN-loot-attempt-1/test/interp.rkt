#lang racket
(require "test-runner.rkt"
         "../parse-interp.rkt"
         "../interp.rkt"
         "../interp-io.rkt")

(test-runner    (λ p (interp (parse p))))
(test-runner-io (λ (s . p) (interp/io (parse p) s)))
