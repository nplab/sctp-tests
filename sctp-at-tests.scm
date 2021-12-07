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

;;; $Id: sctp-at-tests.scm,v 1.9 2014/08/30 19:53:41 tuexen Exp $


;;; Version 1.1.0

;;; History
;;; 26.01.2005 Rename sctp-at-i-2-14 to sctp-at-v-2-14
;;; 06.02.2005 Rename sctp-at-i-2-6 to sctp-at-v-2-6

;;;----------------------------------------------------------
;;; Association Termination (AT)
;;;----------------------------------------------------------


(define (sctp-at-v-2-2 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (peer-addr (get-peer-addr tcb))
	 (header (get-header tcb)))
    ;; terminate it by sending an ABORT chunk.
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    ;; send a HEARTBEAT to see if the peer has removed the TCB.
    (sctp-send header 
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-at-v-2-2 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if an ABORT is returned.



(define (sctp-at-i-2-3 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (local-tsn (get-local-tsn tcb)))
    ;; Send a DATA chunk to the echo server.
    (if (not (= peer-port 13))
	(sctp-send header 
		   (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		   peer-addr))
    (sctp-receive-chunk data-chunk?)
    ;;; wait for at least one retransmission
    (sctp-receive-chunk data-chunk?)
    (sctp-send header
	       (vector (make-sack-chunk (get-peer-tsn tcb) 0 #() #()))
	       peer-addr)
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide a daytime service.
;;; (sctp-at-i-2-3 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed iff the SHUTDOWN is sent only after the SACK is received.



(define (sctp-at-i-2-4 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    ;; Send a DATA chunk to the daytime server.
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
    ;; You should see SHUTDOWNs
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The peer needs to provide a daytime service.
;;; (sctp-at-i-2-4 sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN is retransmitted.


(define (sctp-at-i-2-5 peer-server? peer-addr local-port peer-port)
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
    (if (= peer-port 7)
	(sctp-send header 
		   (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		   peer-addr))
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Acknowledge it.
    (sctp-send header
	       (vector (make-sack-chunk peer-tsn 0 #() #()))
	       peer-addr)
    ;; You should see SHUTDOWNs
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-receive-chunk shutdown-chunk?)
    (sctp-receive-chunk abort-chunk?)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (sctp-receive-chunk abort-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The SUT needs to provide a daytime service.
;;; (sctp-at-i-2-5 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWNs are retransmitted and the HEARTBEAT is not
;;; acknowledged.



(define (sctp-at-v-2-6 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-shutdown-ack-chunk))
	       peer-addr)
    (sctp-receive-chunk shutdown-complete-chunk?)
    stt-test-result-passed))
;;; The SUT needs to be a daytime server.
;;; (sctp-at-v-2-6 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if a SHUTDOWN-COMPLETE is received.



;;;
;;; SCTP-AT-I-2-7-[1234] not implemented because they are API tests only.
;;; They are optional.



(define (sctp-at-i-2-8 peer-server? peer-addr local-port peer-port)
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
	       (vector (make-data-chunk (+mod32 local-tsn 1) 1 1 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk (lambda(chunk) (or (sack-chunk? chunk)
					   (shutdown-chunk? chunk))))
    (sctp-send header
	       (vector (make-data-chunk (+mod32 local-tsn 2) 1 2 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk (lambda(chunk) (or (sack-chunk? chunk)
					   (shutdown-chunk? chunk))))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The SUT needs to privide a daytime service.
;;; (sctp-at-i-2-8 sut-addr tester-port sut-port)
;;; The test is passed if the DATA chunks are acknowledged.



(define (sctp-at-i-2-9 peer-server? peer-addr local-port peer-port)
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
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    ;; Wait for the DATA chunk to receive.
    (sctp-receive-chunk data-chunk?)
    ;; Do not acknowledge it, so the peer has outstanding data.
    ;; Now send a SHUTDOWN chunk and send also a DATA chunk.
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-send header
	       (vector (make-data-chunk (1+mod32 local-tsn) 1 1 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk abort-chunk?)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; The SUT needs to be an echo server.
;;; (sctp-at-i-2-9 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the data sent after the SHUTDOWN is discarded.



(define (sctp-at-i-2-10 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    ;; send data
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    ;; send a SHUTDOWN chunk
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sleep tester-short-wait)
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sleep tester-long-wait)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-unknown))
;;; SUT needs to be an echo server.
;;; (sctp-at-i-2-10 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN-ACK message is not sent.



(define (sctp-at-i-2-11 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    (sctp-receive-chunk data-chunk? header)
    ;; send a SHUTDOWN chunk
    (sctp-send header
	       (vector (make-shutdown-chunk peer-tsn))
	       peer-addr)
    (dotimes (i (1+ sut-maximum-assoc-retransmits))
	     (sctp-receive-chunk shutdown-ack-chunk? header))
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; (sctp-at-i-2-11 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN-ACK message is retransmitted.



(define (sctp-at-i-2-12 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    ;; send a SHUTDOWN chunk
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    ;; do not send a SHUTDOWN-COMPLETE chunk
    (sctp-receive-chunk abort-chunk?)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-passed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-unknown
		stt-test-result-failed))))))
;;; (sctp-at-i-2-12 sut-is-server sut-addr tester-port sut-port)
;;; FIXME: Verify number of SHUTDOWN-ACKs. 
;;; The test is passed iff the HEARTBEAT chunk is responded by an ABORT chunk
;;; indicating that the tag is reflected.



(define (sctp-at-i-2-13 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-peer-tsn tcb)))
    ;; send a SHUTDOWN chunk
    (sctp-send header
	       (vector (make-shutdown-chunk (1-mod32 peer-tsn)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (sctp-send header
	       (vector heartbeat-chunk)
	       peer-addr)
    (let ((result (sctp-receive 1000)))
      (if (equal? result (list #f #f #f #f #f))
	  stt-test-result-failed
	  (let ((chunk (vector-ref (cadr result) 0)))
	    (if (abort-chunk? chunk)
		stt-test-result-passed
		stt-test-result-failed))))))
;;; (sctp-at-i-2-13 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed iff the HEARTBEAT chunk is responded by an ABORT chunk
;;; indicating that the tag is reflected.



(define (sctp-at-v-2-14 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb)))
    ;; send a DATA chunk to the echo server.
    (sctp-send header 
	       (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
	       peer-addr)
    (sctp-receive-chunk data-chunk?)
    ;; send a SHUTDOWN chunk
    (sctp-send header
	       (vector (make-shutdown-chunk (get-peer-tsn tcb)))
	       peer-addr)
    (sctp-receive-chunk shutdown-ack-chunk?)
    (sctp-send header
	       (vector (make-shutdown-complete-chunk #f))
	       peer-addr)
    (sctp-send header
	       (vector (make-abort-chunk #f))
	       peer-addr)
    stt-test-result-passed))
;;; SUT needs to provide an echo service.
;;; (sctp-at-v-2-14 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if the SHUTDOWN-ACK message is transmitted.
