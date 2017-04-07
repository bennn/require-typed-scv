#lang racket/base

(provide
  rts-logger
  log-rts-debug
  log-rts-info
  log-rts-warning
  log-rts-error
  log-rts-fatal)

;; =============================================================================

(define-logger rts)

