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

;;; $Id: sctp-bdc-tests.scm,v 1.5 2010/03/02 12:02:16 tuexen Exp $


;;; Version 1.0



;;;
;;; Bundling of DATA chunks with control chunks. (BDC)
;;;

(define (sctp-bdc-i-7-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let ((local-tag  (choose-local-tag))
	(local-tsn  (random (expt 2 32))))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 tester-os tester-mis local-tsn (if (equal? tester-addr-1 tester-addr-2)
											  (vector)
											  (vector (make-ipv4-address-parameter tester-addr-1)
												  (make-ipv4-address-parameter tester-addr-2))))
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive 500)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  stt-test-result-failed))))
;;; The SUT nees to be a server.
;;; (sctp-bdc-i-7-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT does not respond.



(define (sctp-bdc-i-7-2 peer-server? peer-addr local-port peer-port)
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
		     (vector (make-init-ack-chunk local-tag 1500 tester-os tester-mis local-tsn (vector))
			     (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (let ((result (sctp-receive 500)))
	    (sctp-send header
		       (vector (make-abort-chunk #f))
		       peer-addr)
	    (if (equal? result (list #f #f #f #f #f))
		stt-test-result-passed
		(let ((chunk (vector-ref (cadr result) 0)))
		  (if (or (abort-chunk? chunk)
			  (cookie-echo-chunk? chunk))
		      stt-test-result-passed
		      stt-test-result-failed))))))
      stt-test-result-not-applicable))
;;; The peer needs to be a client.
;;; (sctp-bdc-i-7-2 sut-is-server sut-addr tester-port sut-port)
;;; See ETSI TS.



(define (sctp-bdc-i-7-3 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-shutdown-complete-chunk #f)
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive 2000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (or (abort-chunk? chunk)
		    (shutdown-ack-chunk? chunk))
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-bdc-i-7-3 sut-is-server sut-addr tester-port sut-port)
;;; See ETSI TS.



(define (sctp-bdc-v-7-4 peer-server? peer-addr local-port peer-port)
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
			     (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (sctp-receive-chunk sack-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr))
	stt-test-result-passed)
      stt-test-result-not-applicable))
;;; The peer needs to be a echo server.
;;; (sctp-bdc-v-7-4 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT accepts the DATA.



(define (sctp-bdc-v-7-5 peer-server? peer-addr local-port peer-port)
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
		     (vector (make-cookie-ack-chunk)
			     (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (sctp-receive-chunk sack-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The peer needs to be a client.
;;; (sctp-bdc-v-7-5 sut-addr tester-port sut-port)
;;; The test is passed if the SUT accepts the DATA.



(define (sctp-bdc-v-7-6 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 1500 #() #())
		       (make-shutdown-chunk peer-tsn))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide an echo service.
;;; (sctp-bdc-v-7-6 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SUT accepts the SACK bundled with the SHUTDOWN
;;; and sends a SHUTDOWN-ACK.



(define (sctp-bdc-v-7-7 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-sack-chunk (1-mod32 peer-tsn) 1500 #() #())
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk sack-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-bdc-v-7-7 sut-is-server sut-addr tester-port sut-port)



(define (sctp-bdc-v-7-8 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send (make-common-header local-port peer-port (1+mod32 (get-verification-tag header)))
	       (vector (make-shutdown-ack-chunk)
		       (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (let ((result (sctp-receive 2000)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (let ((chunk (vector-ref (cadr result) 0)))
	(if (or (abort-chunk? chunk)
		(shutdown-chunk? chunk)
		(shutdown-complete-chunk? chunk))
	    stt-test-result-passed
	    stt-test-result-failed)))))

;;; The peer needs to provide a daytime service.
;;; (sctp-bdc-v-7-8 sut-is-server sut-addr tester-port sut-port)
;;; See ETSI TS.
