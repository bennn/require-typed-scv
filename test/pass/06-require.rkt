#lang typed/racket/base
;(require require-typed-scv)
;(require/typed/scv "06-provide.rkt"
;  (struct myNil ())
;  (struct myCons ([hd : Integer] [tl : (U myNil myCons)]))
;  (rev (-> myCons myCons)))
;
;(module+ test
;  (require typed/rackunit)
;  (require/typed racket/contract/base
;    (has-contract? (-> Any Any)))
;
;  (check-false (has-contract? rev))
;  (check-equal? (rev (myCons 1 (myCons 2 (myCons 3 (myNil)))))
;                (myCons 3 (myCons 2 (myCons 1 (myNil))))))
