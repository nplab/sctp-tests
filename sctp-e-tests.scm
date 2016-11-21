;;; 
;;; Copyright (C) 2003, 2004, 2005 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: sctp-e-tests.scm,v 1.4 2009/11/25 21:39:55 tuexen Exp $

;;; Version 1.0

;;;
;;; Error (E)
;;;



(define (sctp-e-o-6-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32))))
	(let* ((answer (sctp-receive-chunk init-chunk?))
	       (init     (vector-ref (cadr answer) 0))
	       (local-port (get-destination-port (car answer)))
	       (peer-port  (get-source-port (car answer)))
	       (peer-addr (cadddr answer))
	       (parameters (get-parameters init))
	       (peer-tag (get-initiate-tag init))
	       (header (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn (list->vector (cons (make-cookie-parameter (vector 1))
														    (if (equal? tester-addr-1 tester-addr-2)
															(list)
															(list (make-ipv4-address-parameter tester-addr-1)
															      (make-ipv4-address-parameter tester-addr-2)))))))
		     peer-addr)
	  (sctp-receive-chunk cookie-echo-chunk?)
	  (sctp-send header
		     (vector (make-error-chunk (vector (make-cause 3 #(0 1 0 0)))))
		     peer-addr)
	  (let ((result (sctp-receive 500)))
	    (sctp-send header
		       (vector (make-abort-chunk #f))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		(let ((chunk (vector-ref (cadr result) 0)))
		  (if (init-chunk? chunk)
		      stt-test-result-passed
		      stt-test-result-failed))))))
      stt-test-result-not-applicable))
;;; The peer needs to be a client.
;;; (sctp-e-o-6-1 sut-is-server sut-addr tester-port sut-port)
;;; See ETSI TS.



(define (sctp-e-o-6-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (local-tsn (get-local-tsn tcb)))
    (sctp-send header
	       (vector (make-error-chunk (vector (make-cause 3 #(0 0 1 0)))))
	       peer-addr)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (sctp-receive-chunk heartbeat-ack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-e-o-6-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the ERROR is ignored and the HEARTBEAT-ACK sent.



(define (sctp-e-i-6-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (local-tsn (get-local-tsn tcb)))
    (sctp-send header
	       (vector (make-data-chunk local-tsn (- (expt 2 16) 1) 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk (lambda (c) (or (error-chunk? c)
					(abort-chunk? c))))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)))
;;; (sctp-e-i-6-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the second and third DATA chunk are responded with ERROR chunks.



(define (sctp-e-i-6-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32))))
	(let* ((answer (sctp-receive-chunk init-chunk?))
	       (init     (vector-ref (cadr answer) 0))
	       (local-port (get-destination-port (car answer)))
	       (peer-port  (get-source-port (car answer)))
	       (peer-addr (cadddr answer))
	       (peer-tag     (get-initiate-tag init))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn (list->vector
												 (if (equal? tester-addr-1 tester-addr-2)
												     (list)
												     (list (make-ipv4-address-parameter tester-addr-1)
													   (make-ipv4-address-parameter tester-addr-2))))))
		     peer-addr)
	  (let ((result (sctp-receive 1000)))
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		(let ((chunk (vector-ref (cadr result) 0)))
		  (if (or (error-chunk? chunk)
			  (abort-chunk? chunk))
		      stt-test-result-passed
		      stt-test-result-failed))))))
	stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-e-i-6-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an ERROR is returned.



(define (sctp-e-i-6-5 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32))))
	(let* ((answer (sctp-receive-chunk init-chunk?))
	       (init     (vector-ref (cadr answer) 0))
	       (local-port (get-destination-port (car answer)))
	       (peer-port  (get-source-port (car answer)))
	       (peer-addr (cadddr answer))
	       (parameters   (get-parameters init))
	       (peer-tag     (get-initiate-tag init))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn  (list->vector (cons (make-cookie-parameter (vector 1))
														     (if (equal? tester-addr-1 tester-addr-2)
															 (list (make-parameter #xc0ab (vector)))
															 (list (make-ipv4-address-parameter tester-addr-1)
															       (make-ipv4-address-parameter tester-addr-2)
															       (make-parameter #xc0ab (vector))))))))
		     peer-addr)
	  (sctp-receive-chunk error-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)))
      stt-test-result-not-applicable))
;;; The peer needs to be a client. 
;;; (sctp-e-i-6-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed iff an INIT-ACK is received without an error.



(define (sctp-e-i-6-6 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn (if (equal? tester-addr-1 tester-addr-2)
											      (vector)
											      (vector (make-ipv4-address-parameter tester-addr-1)
												      (make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag))
	       (cookie       (get-cookie-parameter-cookie state-cookie)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk cookie) 
			     (make-error-chunk (vector (make-cause 8 (vector)))))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (sctp-send header
		     (vector heartbeat-chunk)
		     peer-addr)
	  (sctp-receive-chunk heartbeat-ack-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The peer needs to be a server.
;;; (sctp-e-i-6-6 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends a COOKIE-ACK and
;;; answers the HEARTBEAT.
