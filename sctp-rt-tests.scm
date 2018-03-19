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

;;; $Id: sctp-rt-tests.scm,v 1.8 2014/08/30 19:52:56 tuexen Exp $


;;; Version 1.1

;;;
;;; Retransmission Timer Test Cases (RT)
;;;

;;; History
;;; 06.02.2005 Change sctp-rt-i-11-[23] such that path verification is supported.
;;; 13.02.2005 Change sctp-rt-i-11-[23] such that correct addresses are used.

(define (apply-pairwise op list)
  (map op (reverse (cdr (reverse list))) (cdr list)))

(define (time-quotient-list times)
  (apply-pairwise /
		  (map (lambda (x) (* 1000 x (/ internal-time-units-per-second)))
		       (apply-pairwise - times))))

(define (sctp-rt-i-11-1 peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  ;; set up a asscociation first.
  (let* ((tcb (if peer-server?
		  (associate-from-port peer-addr local-port peer-port tester-os tester-mis)
		  (accept-association tester-os tester-mis)))
	 (header (get-header tcb))
	 (local-tsn (get-local-tsn tcb))
	 (peer-addr (get-peer-addr tcb))
	 (peer-tsn (get-remote-tsn tcb)))
    (sctp-send header 
	       (vector (make-data-chunk local-tsn (if (= upper-layer-protocol ulp-http) 0 1) 0 test-ppid test-message))
	       peer-addr)
    (let ((times (list)))
      (dotimes (i (1+ sut-maximum-assoc-retransmits))
	       (sctp-receive-chunk data-chunk? header)
	       (set! times (cons (get-internal-real-time) times)))
      (sctp-send header
		 (vector (make-abort-chunk #f))
		 peer-addr)
      (let ((quotients (time-quotient-list times)))
	(if (and (>= (apply min quotients) 0.95)
		 (<= (apply max quotients) 3))
	    stt-test-result-passed
	    stt-test-result-failed)))))
;;; (sctp-rt-i-11-1 sut-is-server sut-addr tester-port sut-port)
;;; The test is passed if you do see retransmissions of the DATA chunk with increased timer.
;;; Should I allow the peer to perform an RTT measurement?



(define (sctp-rt-i-11-2 peer-server? peer-addr local-port peer-port)
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
		   peer-addr)
	(let* ((answer       (sctp-receive-chunk init-ack-chunk?))
	       (init-ack     (vector-ref (cadr answer) 0))
	       (parameters   (get-parameters init-ack))
	       (state-cookie (vector-find parameters cookie-parameter?))
	       (peer-tag     (get-initiate-tag init-ack))
	       (peer-tsn     (get-initial-tsn init-ack))
	       (header       (make-common-header local-port peer-port peer-tag)))
	  (sctp-send header 
		     (vector (make-cookie-echo-chunk (get-cookie-parameter-cookie state-cookie)))
		     peer-addr)
	  (sctp-receive-chunk cookie-ack-chunk?)
	  (let* ((answer    (sctp-receive-chunk heartbeat-chunk?))
		 (heartbeat (vector-ref (cadr answer) 0))
		 (peer-addr (cadddr answer))
		 (local-addr (caddr answer)))
	    (sctp-send header
		       (vector (make-heartbeat-ack-chunk (get-heartbeat-parameter heartbeat)))
		       peer-addr
		       local-addr))
	  (let* ((answer    (sctp-receive-chunk heartbeat-chunk?))
		 (heartbeat (vector-ref (cadr answer) 0))
		 (peer-addr (cadddr answer))
		 (local-addr (caddr answer)))
	    (sctp-send header
		       (vector (make-heartbeat-ack-chunk (get-heartbeat-parameter heartbeat)))
		       peer-addr
		       local-addr))
	  (sctp-send header
		     (vector (make-data-chunk local-tsn 1 0 test-ppid test-message))
		     peer-addr)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-receive-chunk data-chunk?)
	  (sctp-send header
		     (vector (make-abort-chunk #f))
		     peer-addr)
	  stt-test-result-passed))
      stt-test-result-not-applicable))
;;; The peer needs to be an echo server.
;;; (sctp-rt-i-11-2 sut-is-server sut-addr tester-port sut-port tester-addr-1 tester-addr-2)
;;; See ETSI TS.



(define sctp-rt-i-11-3 sctp-rt-i-11-2)
;;; The peer needs to be an echo server.
;;; (sctp-rt-i-11-3 sut-addr tester-port sut-port tester-addr-1 tester-addr-2)
;;; See ETSI TS.

