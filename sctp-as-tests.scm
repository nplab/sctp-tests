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

;;; $Id: sctp-as-tests.scm,v 1.13 2014/08/30 19:46:31 tuexen Exp $


;;; Version 1.1.3

;;; History
;;; 06.12.2004 Move all SUT config data in separate file.
;;; 06.12.2004 Use tester-os and tester-mis
;;; 06.12.2004 Use always 1500 as a_rwnd
;;; 06.12.2004 Do not use random port number in sctp-as-v-1-11-1
;;; 26.01.2005 Rename sctp-as-i-1-7-5 to sctp-as-v-1-7-5
;;; 26.01.2005 Rename sctp-as-v-1-13-2 to sctp-as-i-1-13-2
;;; 26.01.2005 sctp-as-v-1-11-1 changed to send back a correct ABORT
;;; 26.01.2005 In sctp-as-i-1-15 the SUT needs to be a client.
;;; 21.02.2005 Add a missing sctp-reset in sctp-as-v-1-7-3

;;;----------------------------------------------------------
;;; Association Setup (AS)
;;;----------------------------------------------------------

(define (sctp-as-v-1-1-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((tcb (accept-association tester-os tester-mis))
	     (header (get-header tcb))
	     (peer-addr (get-peer-addr tcb)))
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
      
;;; The SUT needs to be a client
;;; (sctp-as-v-1-1-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a full four way handshake is performed



(define (sctp-as-v-1-1-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let* ((answer (associate-from-port peer-addr local-port peer-port tester-os tester-mis))
	     (header (car answer)))
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
	
;;; The SUT needs to be a server. 
;;; (sctp-as-v-1-1-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a full four way handshake is performed.



(define (sctp-as-i-1-2-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(dotimes (i sut-maximum-init-retransmits)
		 (sctp-receive-chunk init-chunk?))
	(sctp-send local-header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client
;;; (sctp-as-i-1-2-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT retransmits the INIT message.



(define (sctp-as-i-1-2-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 
						(list->vector (cons (make-cookie-parameter (vector 1))
								    (if (equal? tester-addr-1 tester-addr-2)
									(list)
									(list (make-ipv4-address-parameter tester-addr-1)
									      (make-ipv4-address-parameter tester-addr-2)))))))
		   peer-addr)
	(sctp-receive-chunk cookie-echo-chunk?)
	(dotimes (i sut-maximum-init-retransmits)
		 (sctp-receive-chunk cookie-echo-chunk?))
	(sctp-send local-header 
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-2-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the COOKIE-ECHO message is retransmitted.



(define sctp-as-i-1-3-1 sctp-as-i-1-2-1)
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-3-1)
;;; The test is passed if the INIT message is retransmitted only
;;; a limited number of times. Please be aware of an upper layer
;;; reinitiating imeadiately. Then you need to look at the
;;; times between the INITs. FIXME: Count number of retransmission.



(define sctp-as-i-1-3-2 sctp-as-i-1-2-2)
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-3-2)
;;; The test is passed if the COOKIE-ECHO message is retransmitted only
;;; a limited number of times. FIXME: Count number of retranmissions



(define (sctp-as-i-1-4-help peer-server? peer-addr local-port peer-port n)
  (sctp-cleanup)
  (if peer-server?
      (begin 
	(dotimes(i n)
		(let ((local-tag  (choose-local-tag))
		      (local-tsn  (random (expt 2 32))))
		  ;; The random number local-port can be 0, which is an invalid source port.
		  ;; Just try the random number...
		  (if (positive? local-port)
		      (sctp-send (make-common-header local-port peer-port 0)
				 (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn (if (equal? tester-addr-1 tester-addr-2)
													    (vector)
													    (vector (make-ipv4-address-parameter tester-addr-1)
														    (make-ipv4-address-parameter tester-addr-2)))))
				 peer-addr)
		      ;; Wait for the INIT-ACK chunk but do not send a COOKIE-ECHO chunk.
		      (sctp-receive-chunk init-ack-chunk?))))
	stt-test-result-passed)
      stt-test-result-not-applicable))

(define (sctp-as-i-1-4 peer-server? peer-addr local-port peer-port)
  (sctp-as-i-1-4-help peer-server? peer-addr local-port peer-port 1000))
;;; The SUT needs to be a server.
;;; n is the number of INITs send.     
;;; (sctp-as-i-1-4-help sut-addr tester-port sut-port 10000)
;;; The test is passed if the SUT does not uses more and more memory.



(define (bit-count vtags)
  (if (null? vtags)
      (make-vector 32 0)
      (let ((count (bit-count (cdr vtags))))
	(dotimes (i 32)
	    (if (= (modulo (quotient (car vtags) (expt 2 i)) 2) 1)
		(vector-set! count i (+ (vector-ref count i) 1))))
	count)))

(define (bit-test vtags)
  (let ((probabilities (map (lambda (x) (/ x (length vtags))) (vector->list (bit-count vtags)))))
    (and (> (apply min probabilities) 1/4)
	 (< (apply max probabilities) 3/4))))
    
(define (randomness-good-enough? vtags)
  (and (bit-test vtags)))

(define (sctp-as-v-1-5-1-help n)
  (sctp-cleanup)
  (do ((i 0 (+ i 1))
       (taglist (list)))
      ((= i n) taglist)
    (let* ((tcb (accept-association tester-os tester-mis))
	   (header (get-header tcb))
	   (peer-addr (get-peer-addr tcb)))
      (set! taglist (cons (get-verification-tag header) taglist))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr))))

(define (sctp-as-v-1-5-1 peer-server? peer-addr local-port peer-port)
  (if (not peer-server?)
      (let ((taglist (sctp-as-v-1-5-1-help 100)))
	(if (randomness-good-enough? taglist)
	    stt-test-result-passed
	    stt-test-result-failed))
      stt-test-result-not-applicable))
;;; The IUT needs to be a client continously reestablishing the 
;;; association.
;;; (sctp-as-v-1-5-1 4)
;;; The test is passed if the IUT uses 'random' initiate tags.
;;; The list of tags is returned, n is the number of accepted assocs.



(define (sctp-as-v-1-5-2-help peer-addr local-port peer-port n)
  (sctp-cleanup)
  (let* ((answer (associate-from-port peer-addr local-port peer-port tester-os tester-mis))
	 (header (car answer)))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    (do ((i 0 (+ i 1))
	 (taglist (list)))
	((= i n) taglist) 
      (sctp-send (make-common-header (get-source-port header)
				     (get-destination-port header)
				     0)
		 (vector (make-init-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 (if (equal? tester-addr-1 tester-addr-2)
											       (vector)
											       (vector (make-ipv4-address-parameter tester-addr-1)
												       (make-ipv4-address-parameter tester-addr-2)))))
		 peer-addr)
      (let ((result (sctp-receive-chunk init-ack-chunk?)))
	(set! taglist (cons (get-initiate-tag (vector-ref (cadr result) 0)) taglist))))))

(define (sctp-as-v-1-5-2 peer-server? peer-addr local-port peer-port)
  (if peer-server?
      (let ((taglist (sctp-as-v-1-5-2-help peer-addr local-port peer-port 100)))
	(if (randomness-good-enough? taglist)
	    stt-test-result-passed
	    stt-test-result-failed))
      stt-test-result-not-applicable))
;;; The IUT needs to be a server. FIXME
;;; (sctp-as-v-1-5-2 sut-addr tester-port sut-port 10)
;;; The test is passed if the IUT uses 'random' initiate tags.



(define (sctp-as-v-1-6-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tsn (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk (random (expt 2 32))
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (vector (make-ipv4-address-parameter tester-addr-1)
						    (make-ipv4-address-parameter tester-addr-2)
						    (make-ipv6-address-parameter (make-ipv6-address "3FFF::1"))
						    (make-cookie-preservative-parameter #xffffffff)
						    (make-ecn-capable-parameter)
						    (make-supported-address-type-parameter (vector 5 6)))))
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))
;;; The IUT needs to be a server.
;;; (sctp-as-v-1-6-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an INIT-ACK message is received.

(define (sctp-as-v-1-6-1-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tsn (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk (random (expt 2 32))
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (vector  (make-ipv4-address-parameter tester-addr-1)
						     (make-supported-address-type-parameter (vector 5 11)))))
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))


(define (sctp-as-v-1-6-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)
							(make-ipv6-address-parameter (make-ipv6-address "3FFF::1"))
							(make-cookie-parameter (make-ascii-bytes "Hallo Peer")))))
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout cookie-echo-chunk? 1000)))
	  (sctp-send local-header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      stt-test-result-passed)))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client
;;; (sctp-as-v-1-6-2)
;;; The test is passed if an COOKIE-ECHO message is received.



(define (sctp-as-v-1-7-1 peer-server? peer-addr local-port peer-port)
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
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (sctp-send header
		     (vector (make-data-chunk (+mod32 local-tsn 0) 0 0 test-ppid test-message #f #t #t)
			     (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #t #t))
		     peer-addr)
	  (sctp-send (make-common-header local-port peer-port local-tag)
		     (vector (make-abort-chunk #t)) peer-addr)
	  stt-test-result-unknown))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server requesting more than tester-mis
;;; outgoing stream. FIXME: Configure Z.
;;; (sctp-as-v-1-7-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the minimum number of streams is negotiated
;;; or the peer sends an ABORT message.



(define (sctp-as-i-1-7-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 0 tester-mis local-tsn (if (equal? tester-addr-1 tester-addr-2)
										      (vector)
										      (vector (make-ipv4-address-parameter tester-addr-1)
											      (make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-7-2 sut-is-peer sut-addr tester-port sut-port)
;;; The test is passed if an ABORT is sent back or the INIT is silently
;;; discarded.



(define (sctp-as-v-1-7-3 peer-server? peer-addr local-port peer-port)
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
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn (list->vector (cons (make-cookie-parameter (vector 1))
														    (if (equal? tester-addr-1 tester-addr-2)
															(list)
															(list (make-ipv4-address-parameter tester-addr-1)
															      (make-ipv4-address-parameter tester-addr-2)))))))
		     peer-addr)
	  (sctp-receive-chunk cookie-echo-chunk?)
	  (sctp-send header
		     (vector (make-cookie-ack-chunk))
		     peer-addr)
	  (sctp-send header
		     (vector (make-data-chunk (+mod32 local-tsn 0) 0 0 test-ppid test-message #f #t #t)
			     (make-data-chunk (+mod32 local-tsn 1) 1 0 test-ppid test-message #f #t #t))
		     peer-addr)
	  (sctp-send (make-common-header local-port peer-port local-tag)
		     (vector (make-abort-chunk #t)) peer-addr)
	  stt-test-result-unknown))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client which requests more than tester-mis streams.
;;; (sctp-as-v-1-7-3 sut-is-peer sut-addr tester-port sut-port)
;;; The test is passed if an ABORT is sent back or the minimum number
;;; of strems is used.



(define (sctp-as-i-1-7-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 0 tester-mis 0
						(list->vector (cons (make-cookie-parameter (make-ascii-bytes "Hallo Peer"))
								    (if (equal? tester-addr-1 tester-addr-2)
									(list)
									(list (make-ipv4-address-parameter tester-addr-1)
									      (make-ipv4-address-parameter tester-addr-2)))))))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (sctp-send local-header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (abort-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-7-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT destroys the TCB and does not send a COOKIE-ECHO
;;; and optionally sends an ABORT.



(define (sctp-as-v-1-7-5 peer-server? peer-addr local-port peer-port)
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
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (and (init-ack-chunk? chunk)
			 (>= (get-mos chunk) 2)
			 (>= (get-mis chunk) 2))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-v-1-7-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an INIT-ACK with at least 2 streams in and out
;;; is received.



(define (sctp-as-i-1-8-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((header (make-common-header local-port peer-port 0))
	    (local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32)))
	    (first-parameter (make-parameter #xc00e (make-ascii-bytes "This is before the parameter")))
	    (second-parameter (make-parameter #xf0aa (make-ascii-bytes "This is the unknown parameter with MSB 11")))
	    (third-parameter (make-parameter #xc00e (make-ascii-bytes "This is after the parameter"))))
	(sctp-send header
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector first-parameter
						    second-parameter
						    third-parameter))) 
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let* ((init-chunk (vector-ref (cadr result) 0))
		     (parameters (vector-filter (get-parameters init-chunk) unrecognized-parameter-parameter?)))
		(if (and (= (vector-length parameters) 3)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 0))
				 first-parameter)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 1))
				 second-parameter)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 2))
				 third-parameter))
		    stt-test-result-passed
		    stt-test-result-failed)))))
	stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-8-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT reports and continues.




(define (sctp-as-i-1-8-1-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (local-tsn (random (expt 2 32)))
	     (peer-port (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag (get-initiate-tag init))
	     (header (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 2 2 local-tsn
						(vector (make-cookie-parameter (vector 1))
							(make-parameter #xc00e (make-ascii-bytes "This is before the parameter"))
							(make-parameter #xf0aa (make-ascii-bytes "This is the unknown parameter with MSB 11"))
							(make-parameter #xc00e (make-ascii-bytes "This is after the parameter")))))
		   peer-addr)
	(sleep tester-short-wait)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t)) 
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-8-1-alt sut-is-server sut-addr tester-port sut-port)


(define (sctp-as-i-1-8-1-alt-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (peer-port (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag (get-initiate-tag init))
	     (local-tsn (random (expt 2 32)))
	     (header (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 2 2 local-tsn
						(vector (make-parameter #xc000 (vector))
							    (make-cookie-parameter (vector 1)))))
		   peer-addr)
	(sleep tester-short-wait)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t)) 
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-8-1-alt-alt sut-is-server sut-addr tester-port sut-port)


(define (sctp-as-i-1-8-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((header (make-common-header local-port peer-port 0))
	    (local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32)))
	    (first-parameter (make-parameter #xc00e (make-ascii-bytes "This is before the parameter")))
	    (second-parameter (make-parameter #x30aa (make-ascii-bytes "This is the unknown parameter with MSB 00")))
	    (third-parameter (make-parameter #xc00e (make-ascii-bytes "This is after the parameter"))))
	(sctp-send header
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector first-parameter
						    second-parameter
						    third-parameter))) 
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let* ((init-ack-chunk (vector-ref (cadr result) 0))
		     (parameters (vector-filter (get-parameters init-ack-chunk) unrecognized-parameter-parameter?)))
		(if (and (= (vector-length parameters) 1)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 0))
				 first-parameter))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-8-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not report and does not continue.



(define (sctp-as-i-1-8-2-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init   (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (local-tsn (random (expt 2 32)))
	     (peer-port  (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag     (get-initiate-tag init))
	     (header       (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 2 2 local-tsn
						(vector (make-cookie-parameter (vector 1))
							(make-parameter #xc00e (make-ascii-bytes "This is before the parameter"))
							(make-parameter #x30aa (make-ascii-bytes "This is the unknown parameter with MSB 00"))
							(make-parameter #xc00e (make-ascii-bytes "This is after the parameter")))))
		   peer-addr)
	(sleep tester-short-wait)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t)) 
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-8-2-alt sut-is-server sut-addr tester-port sut-port)


(define (sctp-as-i-1-8-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((header (make-common-header local-port peer-port 0))
	    (local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32)))
	    (first-parameter (make-parameter #xc00e (make-ascii-bytes "This is before the parameter")))
	    (second-parameter (make-parameter #x70aa (make-ascii-bytes "This is the unknown parameter with MSB 01")))
	    (third-parameter (make-parameter #xc00e (make-ascii-bytes "This is after the parameter"))))
	(sctp-send header
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector first-parameter
						    second-parameter
						    third-parameter))) 
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let* ((init-ack-chunk (vector-ref (cadr result) 0))
		     (parameters (vector-filter (get-parameters init-ack-chunk) unrecognized-parameter-parameter?)))
		(if (and (= (vector-length parameters) 2)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 0))
				 first-parameter)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 1))
				 second-parameter))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-8-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT reports and does not continue.



(define (sctp-as-i-1-8-3-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init   (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (local-tsn (random (expt 2 32)))
	     (peer-port  (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag     (get-initiate-tag init))
	     (header       (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 2 2 local-tsn
						(vector (make-cookie-parameter (vector 1))
							(make-parameter #xc00e (make-ascii-bytes "This is before the parameter"))
							(make-parameter #x70aa (make-ascii-bytes "This is the unknown parameter with MSB 01"))
							(make-parameter #xc00e (make-ascii-bytes "This is after the parameter")))))
		   peer-addr)
	(sleep tester-short-wait)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t)) 
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-8-3-alt sut-is-server sut-addr tester-port sut-port)

(define (sctp-as-i-1-8-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((header (make-common-header local-port peer-port 0))
	    (local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32)))
	    (first-parameter (make-parameter #xc00e (make-ascii-bytes "This is before the parameter")))
	    (second-parameter (make-parameter #xb0aa (make-ascii-bytes "This is the unknown parameter with MSB 10")))
	    (third-parameter (make-parameter #xc00e (make-ascii-bytes "This is after the parameter"))))
	(sctp-send header
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn
					    (vector first-parameter
						    second-parameter
						    third-parameter))) 
		   peer-addr)
	(let ((result (sctp-receive-chunk-with-timeout init-ack-chunk? 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let* ((init-ack-chunk (vector-ref (cadr result) 0))
		     (parameters (vector-filter (get-parameters init-ack-chunk) unrecognized-parameter-parameter?)))
		(if (and (= (vector-length parameters) 2)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 0))
				 first-parameter)
			 (equal? (get-unrecognized-parameter (vector-ref parameters 1))
				 third-parameter))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-8-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not report and continues.



(define (sctp-as-i-1-8-4-alt peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init   (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (local-tsn (random (expt 2 32)))
	     (peer-port  (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag     (get-initiate-tag init))
	     (header       (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 2 2 local-tsn
						(vector (make-cookie-parameter (vector 1))
							(make-parameter #xc00e (make-ascii-bytes "This is before the parameter"))
							(make-parameter #xb0aa (make-ascii-bytes "This is the unknown parameter with MSB 10"))
							(make-parameter #xc00e (make-ascii-bytes "This is after the parameter")))))
		   peer-addr)
	(sleep tester-short-wait)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t)) 
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-8-4-alt sut-is-server sut-addr tester-port sut-port)

(define (sctp-as-o-1-9-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let* ((answer (associate-from-port peer-addr local-port peer-port tester-os tester-mis))
	     (header (car answer)))
	(sctp-send (make-common-header (get-source-port header)
				       (get-destination-port header)
				       0)
		   (vector (make-init-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 (vector)))
		   peer-addr)
	(sctp-receive-chunk init-ack-chunk?)
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-o-1-9-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back an INIT-ACK.



(define (sctp-as-o-1-9-2-help peer-server? peer-addr local-addr-1 local-addr-2 local-addr-3 local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let* ((answer (associate-from-port peer-addr local-port peer-port tester-os tester-mis))
	     (header (car answer)))
	(sctp-send (make-common-header (get-source-port header)
				       (get-destination-port header)
				       0)
		   (vector (make-init-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0  (if (equal? local-addr-1 local-addr-2)
												  (vector (make-ipv4-address-parameter local-addr-1)
													  (make-ipv4-address-parameter local-addr-3))
												  (vector (make-ipv4-address-parameter local-addr-1)
													  (make-ipv4-address-parameter local-addr-2)
													  (make-ipv4-address-parameter local-addr-3)))))
		   peer-addr)
	(sctp-receive-chunk abort-chunk?)
	(sctp-send header
		   (vector heartbeat-chunk)
		   peer-addr)
	(sctp-receive-chunk heartbeat-ack-chunk?)
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))

(define (sctp-as-o-1-9-2 peer-server? peer-addr local-port peer-port)
  (sctp-as-o-1-9-2-help peer-server? peer-addr tester-addr-1 tester-addr-2 tester-addr-3 local-port peer-port))
;;; The SUT needs to be a server.
;;; (sctp-as-o-1-9-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back an ABORT and
;;; responds with an HEARTBEAT-ACK.



(define (sctp-as-v-1-10-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn #()))
		   peer-addr)
	(sctp-receive-chunk init-ack-chunk?)
	(sctp-send (make-common-header local-port peer-port local-tag)
		   (vector (make-abort-chunk #t))
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-v-1-10-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an INIT-ACK is received with the sut-addr
;;; as the source address.



(define (sctp-as-v-1-10-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0
						(list->vector (cons (make-cookie-parameter (make-ascii-bytes "Hallo Peer"))
								    (if (equal? tester-addr-1 tester-addr-2)
									(list)
									(list (make-ipv4-address-parameter tester-addr-1)
									      (make-ipv4-address-parameter tester-addr-2)))))))
		   peer-addr)
	(sctp-receive-chunk cookie-echo-chunk?)
	(sctp-send local-header 
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-v-1-10-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a COOKIE-ECHO is sent to the source
;;; address of the INIT-ACK.



(define (sctp-as-v-1-11-1-help peer-server? peer-address local-addr-1 local-addr-2 local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (if (equal? tester-addr-1 tester-addr-2)
						(vector)
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)))))
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
	  (sleep tester-long-wait)
	  (sctp-send header
		     (vector (make-abort-chunk #f)) peer-address)
	  stt-test-result-unknown))
      stt-test-result-not-applicable))

(define (sctp-as-v-1-11-1 peer-server? peer-address local-port peer-port)
  (sctp-as-v-1-11-1-help peer-server? peer-address tester-addr-1 tester-addr-2 local-port peer-port))
;;; The peer SUT to be a server.
;;; (sctp-as-v-1-11-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends HEARTBEATS to the address used for the
;;; handshake and the additional adddress. FIXME: verdict.
;;; for the handshake.



(define (sctp-as-v-1-11-2-help peer-server? peer-address additional-addr local-port peer-port) 
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-address (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 
						(vector (make-cookie-parameter (make-ascii-bytes "Hallo Peer"))
							(make-ipv4-address-parameter additional-addr))))
		   peer-address)
	(sctp-receive-chunk cookie-echo-chunk?)
	(sctp-send local-header (vector (make-cookie-ack-chunk)) peer-address)
	(sleep tester-short-wait)
	(sctp-send local-header (vector (make-abort-chunk #f)) peer-address)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
 
(define (sctp-as-v-1-11-2 peer-server? peer-address local-port peer-port)
  (sctp-as-v-1-11-2-help peer-server? peer-address tester-addr-2 local-port peer-port))
;;; The SUT needs to be a client.
;;; (sctp-as-v-1-11-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends HEARTBEATs to all addresses. Please
;;; note that tester-addr-1 has to be used as the source address for the
;;; handshake.



(define (sctp-as-v-1-12-1 peer-server? peer-address local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (vector (make-hostname-parameter "linux-8-2"))			    
					    ))
		   peer-address)
	(sctp-receive-chunk abort-chunk?)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-v-1-12-1 sut-is-server sut-addr tester-port sut-port)
;;; This test is passed if the SUT sends an ABORT indicating an
;;; unresolvable address.



(define (sctp-as-i-1-12-2 peer-server? peer-address local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk (random (expt 2 32)) 1500 tester-os tester-mis 0 
						(vector (make-cookie-parameter (make-ascii-bytes "Hallo Peer"))
							(make-hostname-parameter "linux-8-2"))))
		   peer-addr)
	(sctp-receive-chunk abort-chunk?)
	(sctp-send local-header
		   (vector (make-abort-chunk #t))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-12-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends an ABORT indicating
;;; an unresolvable address.



(define (sctp-as-v-1-13-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (vector (make-supported-address-type-parameter (vector 5 6)))	
					    (if (equal? tester-addr-1 tester-addr-2)
						(vector)
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(sctp-receive-chunk init-ack-chunk?)
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-abort-chunk #t))
		   peer-addr)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-v-1-13-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT only use IPv4 or IPv6 addresses.


(define (sctp-as-i-1-13-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk local-tag 
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    (vector (make-supported-address-type-parameter (vector 6)))	
					    (if (equal? tester-addr-1 tester-addr-2)
						(vector)
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(sctp-receive-chunk (lambda (chunk) (or (abort-chunk? chunk)
					       (init-ack-chunk? chunk))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-abort-chunk #t))
		   peer-addr))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-as-i-1-13-2 sut-is-server sut-addr tester-port sut-port)
;;; This test is my interpretation.
;;; 


(define (sctp-as-i-1-14-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if peer-server?
      (let ((local-tag  (choose-local-tag))
	    (local-tsn  (random (expt 2 32))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk 0
					    1500
					    tester-os
					    tester-mis
					    local-tsn
					    #()))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (abort-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT should be a server.
;;; (sctp-as-i-1-14-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if no INIT-ACK is sent by the SUT.



(define (sctp-as-i-1-14-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (peer-addr (cadddr answer))
	     (init (vector-ref (cadr answer) 0))
	     (peer-tag (get-initiate-tag init))      
	     (peer-header (car answer))
	     (peer-port (get-source-port peer-header))
	     (local-port (get-destination-port peer-header))
	     (local-header (make-common-header local-port peer-port peer-tag)))
	(sctp-send local-header 
		   (vector (make-init-ack-chunk 0 1500 tester-os tester-mis 0
						(list->vector (cons (make-cookie-parameter (make-ascii-bytes "Hello"))
								    (if (equal? tester-addr-1 tester-addr-2)
									(list)
									(list (make-ipv4-address-parameter tester-addr-1)
									      (make-ipv4-address-parameter tester-addr-2)))))))
		   peer-addr)
 	(let ((result (sctp-receive 3000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-passed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (or (abort-chunk? chunk)
			(init-chunk? chunk))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-14-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not send a COOKIE-ECHO. 



(define (sctp-as-i-1-15 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (begin 
	(sleep tester-long-wait)
	stt-test-result-unknown)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-as-i-1-15 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends INITs to both addresses 
