#lang racket/base

(provide
  require-typed-scv-logger
  log-require-typed-scv-debug
  log-require-typed-scv-info
  log-require-typed-scv-warning
  log-require-typed-scv-error
  log-require-typed-scv-fatal)

;; =============================================================================

(define-logger require-typed-scv)

