;;; 
;;; Copyright (C) 2010 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: sctp-misc-tests.scm,v 1.5 2014/11/17 19:53:11 tuexen Exp $

(define (sctp-cookie-ack-from-other-address peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
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
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn (vector (make-cookie-parameter (vector 1))
													(make-ipv4-address-parameter tester-addr-1)
													(make-ipv4-address-parameter tester-addr-2))))
		     peer-addr)
	  (sctp-receive-chunk cookie-echo-chunk?)
	  (sctp-send header
		     (vector (make-cookie-ack-chunk))
		     peer-addr
		     tester-addr-2)
	  (let ((result (sctp-receive 3000)))
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		(let ((chunk (vector-ref (cadr result) 0)))
		  (if (cookie-echo-chunk? chunk)
		      stt-test-result-failed
		      stt-test-result-passed))))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client. 
;;; (sctp-cookie-ack-from-other-address sut-is-server sut-addr tester-port sut-port)

(define (sctp-cookie-ack-bundled-with-abort peer-server? peer-addr local-port peer-port)
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
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie))
			     (make-abort-chunk #t))
		     peer-addr)
	  (let ((result (sctp-receive 3000)))
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		stt-test-result-failed))))
      stt-test-result-not-applicable))

(define (sctp-init-ack-stream-test peer-server? peer-addr local-port peer-port)
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
		     (vector (make-init-ack-chunk local-tag
						  1500
						  (1+ (get-mis init))
						  (get-mos init)
						  local-tsn
						  (list->vector (cons (make-cookie-parameter (vector 1))
								      (if (equal? tester-addr-1 tester-addr-2)
									  (list)
									  (list (make-ipv4-address-parameter tester-addr-1)
										(make-ipv4-address-parameter tester-addr-2)))))))
		     peer-addr)))
      stt-test-result-not-applicable))

(define (sctp-init-with-set-primary-valid-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if peer-server?
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
	    (local-tsn (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 os mis local-tsn
					    (vector (make-set-primary-address-parameter 1 (make-ipv4-address-parameter tester-addr-1)))))
		   peer-addr)
      	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))

(define (sctp-init-with-set-primary-invalid-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if peer-server?
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
	    (local-tsn (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 os mis local-tsn
					    (vector (make-set-primary-address-parameter 1 (make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
      	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))

(define (sctp-init-with-set-primary-hostname-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if peer-server?
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
	    (local-tsn (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 os mis local-tsn
					    (vector (make-set-primary-address-parameter 1 (make-hostname-parameter "stt.sctp.org")))))
		   peer-addr)
      	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))

(define (sctp-init-ack-with-set-primary-valid-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
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
		     (vector (make-init-ack-chunk local-tag 1500 os mis local-tsn
						  (vector (make-set-primary-address-parameter 1 (make-ipv4-address-parameter tester-addr-1))
							  (make-cookie-parameter (vector 1)))))
		     peer-addr)
	  (let ((result (sctp-receive-chunk-with-timeout cookie-echo-chunk? 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f #()))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-failed
		stt-test-result-passed))))
      stt-test-result-not-applicable))


(define (sctp-init-ack-with-set-primary-invalid-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
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
		     (vector (make-init-ack-chunk local-tag 1500 os mis local-tsn
						  (vector (make-set-primary-address-parameter 1 (make-ipv4-address-parameter tester-addr-2))
							  (make-cookie-parameter (vector 1)))))
		     peer-addr)
	  (let ((result (sctp-receive-chunk-with-timeout cookie-echo-chunk? 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f #()))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-failed
		stt-test-result-passed))))
      stt-test-result-not-applicable))

(define (sctp-init-ack-with-set-primary-hostname-address peer-server? peer-addr local-port peer-port)
  (sctp-reset)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (os 1)
	    (mis 65535)
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
		     (vector (make-init-ack-chunk local-tag 1500 os mis local-tsn
						  (vector (make-set-primary-address-parameter 1 (make-hostname-parameter "stt.sctp.org"))
							  (make-cookie-parameter (vector 1)))))
		     peer-addr)
	  (let ((result (sctp-receive-chunk-with-timeout cookie-echo-chunk? 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f #()))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-failed
		stt-test-result-passed))))
      stt-test-result-not-applicable))

(define (sctp-malformed-stream-reset-request peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  #x01020304))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector (make-supported-extensions-parameter (vector 130)))))
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (sctp-send header
		     (vector (make-chunk #x82 0 (vector #x00 #x0d #x01 #x00 #x01 #x02 #x03 #x04)))
		     peer-addr)
	  (sctp-send header
		     (vector heartbeat-chunk)
		     peer-addr)
	  (let ((result (sctp-receive 1000)))
	    (sctp-send header
		       (vector (make-abort-chunk #f))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-failed
		(let ((chunk (vector-ref (cadr result) 0)))
		  (if (or (abort-chunk? chunk)
			  (heartbeat-ack-chunk? chunk))
		      stt-test-result-passed
		      stt-test-result-failed))))))
      stt-test-result-not-applicable))

(define (sctp-cookie-with-wrong-address peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  #x01020304))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector (make-supported-extensions-parameter (vector 130)))))
		   peer-addr tester-addr-1)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie))
			     (make-data-chunk local-tsn 0 0 test-ppid test-message))
		     peer-addr tester-addr-2)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
