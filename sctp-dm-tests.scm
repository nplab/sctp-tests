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

;;; $Id: sctp-dm-tests.scm,v 1.6 2010/03/02 12:02:16 tuexen Exp $

;;;
;;; Duplicate Messages (DM)
;;;

;;;

;;;

(define (sctp-dm-o-4-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((result (sctp-receive-chunk init-chunk?))
	     (local-port (get-destination-port (car result)))
	     (peer-port (get-source-port (car result))))
	(sctp-send (make-common-header local-port peer-port 0)
		   (vector (make-init-chunk (random (expt 2 32)) ; Initiation Tag
					    1500                 ; a_rwnd
					    tester-os            ; MOS
					    tester-mis           ; MIS
					    0                    ; Initial TSN
					    (if (equal? tester-addr-1 tester-addr-2)
						(vector)
						(vector (make-ipv4-address-parameter tester-addr-1)
							(make-ipv4-address-parameter tester-addr-2)))))
		   peer-addr)
	(let ((result (sctp-receive 4000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (init-ack-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-dm-o-4-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an INIT-ACK or ABORT is sent back.
;;; ETSI TS is wrong!



(define (sctp-dm-o-4-2-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (peer-addr (get-peer-addr tcb))
	 (header (get-header tcb)))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk (random (expt 2 32)) ; Initiation Tag
					1500                 ; a_rwnd
					tester-os            ; OS
					tester-mis           ; MIS
					0                    ; Initial TSN
					(if (equal? tester-addr-1 tester-addr-2)
					    (vector)
					    (vector (make-ipv4-address-parameter tester-addr-1)
						    (make-ipv4-address-parameter tester-addr-2)))))
	       peer-addr)
    (sctp-receive-chunk init-ack-chunk?)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (sctp-receive-chunk heartbeat-ack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-dm-o-4-2-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the HEARTBEAT is answered with a HEARTBEAT-ACK.



(define (sctp-dm-o-4-2-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk (random (expt 2 32)) ; Initiation Tag
					1500                 ; a_rwnd
					tester-os            ; MIS
					tester-mis           ; MOS
					0                    ; Initial TSN
					(if (equal? tester-addr-1 tester-addr-2)
					    (vector)
					    (vector (make-ipv4-address-parameter tester-addr-1)
						    (make-ipv4-address-parameter tester-addr-2)))))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (shutdown-ack-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-dm-o-4-2-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN-ACK is retransmitted and the INIT is ignored.



(define (sctp-dm-o-4-3 peer-server? peer-addr local-port peer-port)
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
															 (list)
															 (list (make-ipv4-address-parameter tester-addr-1)
															       (make-ipv4-address-parameter tester-addr-2)))))))
		     peer-addr)
	  (sctp-receive-chunk cookie-echo-chunk?)
	  (sctp-send header
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn  (list->vector (cons (make-cookie-parameter (vector 2))
														     (if (equal? tester-addr-1 tester-addr-2)
															 (list)
															 (list (make-ipv4-address-parameter tester-addr-1)
															       (make-ipv4-address-parameter tester-addr-2)))))))
		     peer-addr)
	  (sctp-receive-chunk cookie-echo-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-dm-o-4-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the retransmitted  INIT-ACK is discarded.



(define (sctp-dm-o-4-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((tcb (accept-association tester-os tester-mis))
	     (header (get-header tcb))
	     (peer-addr (get-peer-addr tcb)))
	(sctp-send header
		   (vector (make-cookie-ack-chunk))
		   peer-addr)
	(sctp-send header
		   (vector heartbeat-chunk)
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr))
	(if (equal? result (list #f #f #f #f #f))
	    stt-test-result-failed
	    (let ((chunk (vector-ref (cadr result) 0)))
	      (if (heartbeat-ack-chunk? chunk)
		  stt-test-result-passed
		  stt-test-result-failed))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-dm-o-4-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a HEARTBEAT-ACK is returned.


    
(define (sctp-dm-o-4-5 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Send a DATA chunk to the echo server.
    (if (not (= peer-port 13))
	(sctp-send header 
		   (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		   peer-addr))
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    ;; Wait for the SHUTDOWN chunk and send a SHUTDOWN-ACK
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send header
	       (vector (make-shutdown-chunk peer-tsn))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The SUT needs to provide a daytime service.
;;; (sctp-dm-o-4-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends a SHUTDOWN-ACK.



(define (sctp-dm-o-4-6-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (peer-port (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag (get-initiate-tag init))
	     (peer-tsn (get-initial-tsn init))
	     (header (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
		   peer-addr)
	(let ((result (sctp-receive 4000)))
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let ((chunk (vector-ref (cadr result) 0)))
		(if (init-chunk? chunk)
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; The SUT needs to be a client 
;;; (sctp-dm-o-4-6-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT ignores the SHUTDOWN and retransmits the INIT.



(define (sctp-dm-o-4-6-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let ((verification-tag (random (expt 2 32)))
	(tsn (random (expt 2 32))))
    (sctp-send (make-common-header local-port peer-port verification-tag)
	       (vector (make-shutdown-chunk tsn))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; The SUT needs to be a server.
;;; (sctp-dm-o-4-6-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends back an ABORT with T-bit set.



(define (sctp-dm-o-4-6-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (if (not (= peer-port 13))
	(sctp-send header 
		   (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		   peer-addr))
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    ;; Wait for the SHUTDOWN chunk and send a SHUTDOWN-ACK
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send header
	       (vector (make-shutdown-chunk peer-tsn))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide a daytime service.
;;; (sctp-dm-o-4-6-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT (re)transmits SHUTDOWN-ACK.



(define (sctp-dm-o-4-7-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init     (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (peer-port  (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag     (get-initiate-tag init))
	     (header       (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-shutdown-ack-chunk))
		   peer-addr)
	(sctp-receive-chunk init-chunk?)
	(sctp-send header
		   (vector (make-abort-chunk #f))
		   peer-addr)
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The SUT needs to be a client.
;;; (sctp-dm-o-4-7-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed iff the peer keeps retransmitting INIT chunks.



(define (sctp-dm-o-4-7-2-established peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (local-tsn (get-local-tsn tcb)))
    ;; Send a SHUTDOWN-ACK chunk to the server.
    (sctp-send header
	       (vector (make-shutdown-ack-chunk))
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
;;; (sctp-dm-o-4-7-2-established sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the HEARTBEAT-ACK is received.



(define (sctp-dm-o-4-7-2-cookie-wait peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((answer (sctp-receive-chunk init-chunk?))
	     (init (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (peer-port (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag (get-initiate-tag init))
	     (header (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-shutdown-ack-chunk))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let ((chunk (vector-ref (cadr result) 0))
		    (header (car result)))
		(if (and (shutdown-complete-chunk? chunk)
			 (equal? (get-t-bit chunk) #t)
			 (equal? (get-verification-tag header) peer-tag))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; (sctp-dm-o-4-7-2-cookie-wait sut-is-server sut-addr tester-port sut-port)



(define (sctp-dm-o-4-7-2-cookie-echoed peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let* ((local-tag (choose-local-tag))
	     (local-tsn (random (expt 2 32)))
	     (answer (sctp-receive-chunk init-chunk?))
	     (init (vector-ref (cadr answer) 0))
	     (local-port (get-destination-port (car answer)))
	     (peer-port (get-source-port (car answer)))
	     (peer-addr (cadddr answer))
	     (peer-tag (get-initiate-tag init))
	     (header (make-common-header local-port peer-port peer-tag)))
	(sctp-send header
		   (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn  (list->vector (cons (make-cookie-parameter (vector 1))
														   (if (equal? tester-addr-1 tester-addr-2)
														       (list)
														       (list (make-ipv4-address-parameter tester-addr-1)
															     (make-ipv4-address-parameter tester-addr-2)))))))
		 peer-addr)
	(sctp-receive-chunk cookie-echo-chunk?)
	(sctp-send header
		   (vector (make-shutdown-ack-chunk))
		   peer-addr)
	(let ((result (sctp-receive 1000)))
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  (if (equal? result (list #f #f #f #f #f))
	      stt-test-result-failed
	      (let ((chunk (vector-ref (cadr result) 0))
		    (header (car result)))
		(if (and (shutdown-complete-chunk? chunk)
			 (equal? (get-t-bit chunk) #t)
			 (equal? (get-verification-tag header) peer-tag))
		    stt-test-result-passed
		    stt-test-result-failed)))))
      stt-test-result-not-applicable))
;;; (sctp-dm-o-4-7-2-cookie-echoed sut-is-server sut-addr tester-port sut-port)



(define sctp-dm-o-4-7-2 sctp-dm-o-4-7-2-established)
;;; (sctp-dm-o-4-7-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the HEARTBEAT-ACK is received.



(define (sctp-dm-o-4-7-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-shutdown-ack-chunk))
	       peer-addr)
    (sctp-receive-chunk shutdown-complete-chunk?)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (sctp-receive-chunk abort-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-dm-o-4-7-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT sends a SHUTDOWN-COMPLETE chunk
;;; and answers the HEARTBAT with an ABORT with the T-bit set.



(define (sctp-dm-o-4-8 peer-server? peer-addr local-port peer-port)
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
		     (vector (make-cookie-echo-chunk cookie))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (vector-set! cookie 2 (modulo (+ (vector-ref cookie 2) 1) 256))
	  (sctp-send header
		     (vector (make-cookie-echo-chunk cookie))
		     peer-addr)
	  (sctp-send header
		     (vector heartbeat-chunk)
		     peer-addr)
	  (sctp-receive-chunk heartbeat-ack-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The SUT needs to be a server.
;;; (sctp-dm-o-4-8 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the COOKIE-ECHO is ignored.



(define (sctp-dm-o-4-9 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (if (not peer-server?)
      (let ((local-tag (choose-local-tag))
	    (local-tsn (random (expt 2 32))))
	(let* ((answer (sctp-receive-chunk init-chunk?))
	       (init (vector-ref (cadr answer) 0))
	       (local-port (get-destination-port (car answer)))
	       (peer-port (get-source-port (car answer)))
	       (peer-addr (cadddr answer))
	       (peer-tag (get-initiate-tag init))
	       (header (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header
		     (vector (make-shutdown-complete-chunk #f))
		     peer-addr)
	  (sctp-receive-chunk init-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The peer needs to be a client.
;;; (sctp-dm-o-4-9 sut-is-server sut-addr tester-port sut-port)



(define (sctp-dm-o-4-10 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-dm-o-4-10 sut-is-server sut-addr tester-port sut-port)
;;; Test test is passed if the DATA is not accepted.
