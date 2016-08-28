;;; 
;;; Copyright (C) 2006-2010 M. Tuexen, tuexen@fh-muenster.de
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




(define (sctp-send-bundled-heartbeat-chunks-help peer-server? peer-addr local-port peer-port number-of-chunks)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (chunk  (make-heartbeat-chunk (make-heartbeat-parameter (vector))))
	 (chunks (make-vector number-of-chunks))
	 (header (car tcb))
	 (peer-address (cadddr tcb)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks peer-address)
    (sctp-receive-chunk heartbeat-ack-chunk?)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (heartbeat-ack-chunk? chunk)
		stt-test-result-failed
		stt-test-result-passed))))))
;;; (sctp-send-bundled-heartbeat-chunks-help sut-is-server sut-addr tester-port sut-port 10)

(define (sctp-bundled-heartbeats peer-server? peer-addr local-port peer-port)
  (sctp-send-bundled-heartbeat-chunks-help  peer-server? peer-addr local-port peer-port 10))

(define (sctp-send-bundled-unknown-chunks-help peer-server? peer-addr local-port peer-port number-of-chunks)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (chunk  (make-chunk #xf7 0 (vector)))
	 (chunks (make-vector number-of-chunks))
	 (header (car tcb))
	 (peer-address (cadddr tcb)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks peer-address)
    (sctp-receive-chunk error-chunk?)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (error-chunk? chunk)
		stt-test-result-failed
		stt-test-result-passed))))))

;;;(sctp-send-bundled-unknown-chunks-help sut-is-server sut-addr tester-port sut-port 10)
(define (sctp-bundled-unknowns peer-server? peer-addr local-port peer-port)
  (sctp-send-bundled-unknown-chunks-help  peer-server? peer-addr local-port peer-port 10))

(define (sctp-send-bundled-shutdown-ack-chunks-help peer-server? peer-addr local-port peer-port number-of-chunks)
  (sctp-reset)
  (let* ((header (make-common-header local-port peer-port 1))
	 (chunk  (make-shutdown-ack-chunk))
	 (chunks (make-vector number-of-chunks)))
    (dotimes (i number-of-chunks)
	     (vector-set! chunks i chunk))
    (sctp-send header chunks peer-addr)
    (sctp-receive-chunk shutdown-complete-chunk?)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (shutdown-complete-chunk? chunk)
		stt-test-result-failed
		stt-test-result-passed))))))

;;;(sctp-send-bundled-shutdown-ack-chunks-help sut-is-server sut-addr tester-port sut-port 10)
(define (sctp-bundled-shutdown-acks peer-server? peer-addr local-port peer-port)
  (sctp-send-bundled-shutdown-ack-chunks-help peer-server? peer-addr local-port peer-port 10))
