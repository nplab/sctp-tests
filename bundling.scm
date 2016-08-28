;;; 
;;; Copyright (C) 2006 M. Tuexen, tuexen@fh-muenster.de
;;;
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the
;;; following conditions are met:
;;; 1. Redistributions of source code must retain the above
;;;    copyright notice, this list of conditions and the
;;;    following disclaimer.
;;; 2. Redistributions in binary form must reproduce the
;;;    above copyright notice, this list of conditions and
;;;    the following disclaimer in the documentation and/or
;;;    other materials provided with the distribution.
;;; 3. Neither the name of the project nor the names of
;;;    its contributors may be used to endorse or promote
;;;    products derived from this software without specific
;;;    prior written permission.
;;;  
;;; THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS
;;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
;;; OF SUCH DAMAGE.


(define sut-addr (make-ipv4-address "172.29.96.100"))
(define sut-port 3868)
(define tester-addr (make-ipv4-address "172.29.96.4"))
(define tester-port 3868)

(use-modules (ice-9 syncase))

(define-syntax dotimes 
   (syntax-rules () 
     ((_ (var n res) . body) 
      (do ((limit n) 
           (var 0 (+ var 1))) 
          ((>= var limit) res) 
        . body)) 
     ((_ (var n) . body) 
      (do ((limit n) 
           (var 0 (+ var 1))) 
          ((>= var limit)) 
        . body))))

(define (vector-find vector predicate)
  (if (vector? vector)
      (do ((index 0 (+ index 1)))
          ((or (= index (vector-length vector))
               (predicate (vector-ref vector index)))
           (if (= index (vector-length vector))
               #f
               (vector-ref vector index))))
      #f))
  
(define (sctp-receive-chunk predicate)
  (let* ((result (sctp-receive))
         (chunks (cadr result)))
    (if (vector-find chunks predicate)
        result
        (sctp-receive-chunk predicate))))

(define (associate peer-address peer-port)
  (let ((local-port tester-port)
	(local-tag  (random (expt 2 32)))
	(local-tsn  (random (expt 2 32))))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 1 1 local-tsn #()))
	       peer-address)
    (let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	   (init-ack     (vector-ref (cadr answer) 0))
	   (parameters   (get-parameters init-ack))
	   (state-cookie (vector-find parameters cookie-parameter?))
	   (peer-tag     (get-initiate-tag init-ack))
	   (header       (make-common-header local-port peer-port peer-tag)))
      (sctp-send header 
		 (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		 peer-address)
      (sctp-receive-chunk cookie-ack-chunk?)
      (list header local-tsn (get-initial-tsn init-ack)))))


(define (send-bundled-heartbeat-chunks number-of-chunks remote-address remote-port)
  (sctp-reset)
  (let* ((answer (associate remote-address remote-port))
	 (chunk  (make-heartbeat-chunk (make-heartbeat-parameter (vector))))
	 (chunks (make-vector number-of-chunks))
	 (header (car answer)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks remote-address)
    (sleep 1)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       remote-address)))
;;; (send-bundled-heartbeat-chunks 8 sut-addr sut-port)

(define (send-bundled-unknown-chunks number-of-chunks remote-address remote-port)
  (sctp-reset)
  (let* ((answer (associate remote-address remote-port))
	 (chunk  (make-chunk #xf7 0 (vector)))
	 (chunks (make-vector number-of-chunks))
	 (header (car answer)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks remote-address)
    (sleep 1)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       remote-address)))

;;;(send-bundled-unknown-chunks 8000 sut-addr sut-port)

(define (send-bundled-shutdown-ack-chunks number-of-chunks local-address local-port remote-address remote-port)
  (sctp-reset)
  (let* ((header (make-common-header local-port remote-port 1))
	 (chunk  (make-shutdown-ack-chunk))
	 (chunks (make-vector number-of-chunks)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks remote-address local-address)))

;;;(send-bundled-shutdown-ack-chunks 1000 tester-addr tester-port sut-addr sut-port)
