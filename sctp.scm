;;; 
;;; Copyright (c) 2003-2012 Michael Tuexen
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

;;; $Id: sctp.scm,v 1.21 2014/08/30 19:50:20 tuexen Exp $


;;; Version 1.1.1
;;;
;;; History
;;; 06.12.2004 associate now takes os and mis.
;;; 06.12.2004 associate-from-port now takes os and mis
;;; 06.12.2004 accept-association now takes os and mis
;;; 06.12.2004 associate calls associate-from-port
;;; 07.12.2004 heartbeat message added.
;;; 19.07.2006 make-m3ua-beat-message added.

(define ulp-diameter     1)
(define ulp-echo         2)
(define ulp-http         3)
(define ulp-m3ua         4)
(define ulp-s1ap         5)

(define diameter-port 3868)
(define echo-port        7)
(define http-port       80)
(define m3ua-port     2905)

(define diameter-ppid   46)
(define echo-ppid       38)
(define http-ppid       63)
(define m3ua-ppid        3)
(define s1ap-ppid       18)

(define heartbeat-mode-discard 1)
(define heartbeat-mode-confirm 2)
(define heartbeat-mode-reflect 3)

(define get-header car)
(define get-local-tsn cadr)
(define get-remote-tsn caddr)
(define get-peer-tsn caddr)
(define get-peer-addr cadddr)

(define stt-test-result-passed 0)
(define stt-test-result-failed 1)
(define stt-test-result-unknown 2)
(define stt-test-result-not-applicable 253)

(define secondary-path-confirmed #f)

(define (vector-filter vector predicate)
  (list->vector (filter predicate (vector->list vector))))

(define (vector-find vector predicate)
  (if (vector? vector)
      (do ((index 0 (+ index 1)))
	  ((or (= index (vector-length vector))
	       (predicate (vector-ref vector index)))
	   (if (= index (vector-length vector))
	       #f
	       (vector-ref vector index))))
      #f))

(define* (sctp-receive-chunk predicate #:optional header)
  (let* ((result (sctp-receive))
	 (chunks (cadr result))
	 (local-addr (caddr result))
	 (peer-addr (cadddr result)))
    (cond
     ((and (not (equal? header #f))
	   (= (vector-length chunks) 1)
	   (heartbeat-chunk? (vector-ref chunks 0))
	   (or (= tester-heartbeat-mode heartbeat-mode-reflect)
	       (and (= tester-heartbeat-mode heartbeat-mode-confirm)
		    (not secondary-path-confirmed)
		    (equal? local-addr tester-addr-2)
		    (not (equal? tester-addr-1 tester-addr-2)))))
      (sctp-send header
		 (vector (make-heartbeat-ack-chunk (get-heartbeat-parameter (vector-ref chunks 0))))
		 peer-addr
		 local-addr)
      (if (= tester-heartbeat-mode heartbeat-mode-confirm)
	  (set! secondary-path-confirmed #t))
      (sctp-receive-chunk predicate header))
     ((vector-find chunks predicate)
      result)
     (else
      (sctp-receive-chunk predicate header)))))

(define (sctp-receive-chunk-with-timeout predicate timeout)
  (let* ((result (sctp-receive timeout))
	 (chunks (cadr result)))
    (if (or (equal? result (list #f #f #f #f #f))
	    (vector-find chunks predicate))
	result
	(sctp-receive-chunk-with-timeout predicate timeout))))

(define local-tag #x01020304)

(define (sctp-cleanup)
  (if (not (= sut-port 0))
      (begin
        (sctp-send (make-common-header tester-port sut-port local-tag)
		   (vector (make-abort-chunk #t))
		   sut-addr)))
  (sctp-reset)
  (if (not sut-is-server)
      (let* ((answer (sctp-receive))
	     (local-port (get-destination-port (car answer)))
	     (peer-port (get-source-port (car answer)))
	     (local-addr (caddr answer))
	     (peer-addr (cadddr answer))
	     (chunks (cadr answer)))
	(if (and (= (vector-length chunks) 1)
		 (init-chunk? (vector-ref chunks 0)))
	    (sctp-send (make-common-header local-port peer-port (get-initiate-tag (vector-ref chunks 0)))
		       (vector (make-abort-chunk #f))
		       peer-addr
		       local-addr)
	    (sctp-send (make-common-header local-port peer-port (get-verification-tag (car answer)))
		       (vector (make-abort-chunk #t))
		       peer-addr
		       local-addr))))
  (sctp-reset)
  (set! secondary-path-confirmed #f))

(define (choose-local-tag)
   local-tag)
;;;(random (expt 2 32)))

(define (choose-tag)
  (1+ (random (1- (expt 2 32)))))

(define (associate peer-addr peer-port os mis)
  (associate-from-port peer-addr (random (expt 2 16)) peer-port os mis))

(define (associate-from-port peer-addr local-port peer-port os mis)
  (let ((local-tag  (choose-local-tag))
	(local-tsn  (random (expt 2 32))))
    (sctp-send (make-common-header local-port peer-port 0)
	       (vector (make-init-chunk local-tag 1500 os mis local-tsn (if (equal? tester-addr-1 tester-addr-2)
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
      (sleep tester-handshake-wait)
      (cond
       ((= upper-layer-protocol ulp-echo)
	(list header local-tsn (get-initial-tsn init-ack) peer-addr))
       ((= upper-layer-protocol ulp-http)
	(list header local-tsn (get-initial-tsn init-ack) peer-addr))
       ((= upper-layer-protocol ulp-m3ua)
	(list header local-tsn (get-initial-tsn init-ack) peer-addr))
       ((= upper-layer-protocol ulp-diameter)
	(sctp-send header
		   (vector (make-data-chunk (+mod32 local-tsn 0) 0 0
					    diameter-ppid
					    diameter-capability-exchange-request
					    #f #t #t))
		   peer-addr)
	(sctp-receive-chunk data-chunk?)
	(sctp-send header
		   (vector (make-sack-chunk (get-initial-tsn init-ack) 1500 #() #()))
		   peer-addr)
	(if diameter-sut-sends-initial-watchdog-request
	    (let* ((result (sctp-receive-chunk data-chunk?))
		   (data-chunk (vector-find (cadr result) data-chunk?))
		   (watchdog-request (vector->list (get-user-data data-chunk))))
	      (sctp-send header
			 (vector (make-sack-chunk (+mod32 (get-initial-tsn init-ack) 1) 1500 #() #()))
			 peer-addr)
;;;           Don't respond with an answer, since this might trigger another request. Not sure why...
;;;	      (sctp-send header
;;;			 (vector (make-data-chunk (+mod32 local-tsn 1) 0 1
;;;						  diameter-ppid
;;;						  (list->vector (make-device-watchdog-answer-from-request watchdog-request
;;;													  diameter-origin-host
;;;													  diameter-origin-realm))
;;;						  #f #t #t))
;;;			 peer-addr)
;;;	      (sctp-receive-chunk sack-chunk?)
;;;	      (list header (+mod32 local-tsn 2) (+mod32 (get-initial-tsn init-ack) 2) peer-addr))
	      (list header (+mod32 local-tsn 1) (+mod32 (get-initial-tsn init-ack) 2) peer-addr))
	    (list header (+mod32 local-tsn 1) (+mod32 (get-initial-tsn init-ack) 1) peer-addr)))
       (else
	(error "Unsupported upper layer protocol:" upper-layer-protocol))))))

(define (accept-association os mis)
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
		 (vector (make-init-ack-chunk local-tag 1500 os mis local-tsn (list->vector (cons (make-cookie-parameter (vector 1))
												  (if (equal? tester-addr-1 tester-addr-2)
												      (list)
												      (list (make-ipv4-address-parameter tester-addr-1)
													    (make-ipv4-address-parameter tester-addr-2)))))))
		 peer-addr)
      (sctp-receive-chunk cookie-echo-chunk?)
      (sctp-send header
		 (vector (make-cookie-ack-chunk))
		 peer-addr)
      (sleep tester-handshake-wait)
      (cond 
       ((= upper-layer-protocol ulp-echo)
	(list header local-tsn (get-initial-tsn init) peer-addr))
       ((= upper-layer-protocol ulp-http)
	(list header local-tsn (get-initial-tsn init) peer-addr))
       ((= upper-layer-protocol ulp-m3ua)
	(sctp-receive-chunk data-chunk?)
	(sctp-send header
		   (vector (make-sack-chunk (get-initial-tsn init) 1500 #() #())
			   (make-data-chunk (+mod32 local-tsn 0) 0 0 test-ppid m3ua-aspup-ack-message #f #t #t)
			   (make-data-chunk (+mod32 local-tsn 1) 0 1 test-ppid m3ua-ntfy-inactive-message #f #t #t))
		   peer-addr)
	(sctp-receive-chunk data-chunk?)
	(sctp-send header
		   (vector (make-sack-chunk (1+mod32 (get-initial-tsn init)) 1500 #() #())
			   (make-data-chunk (+mod32 local-tsn 2) 0 2 test-ppid m3ua-aspac-ack-message #f #t #t)
			   (make-data-chunk (+mod32 local-tsn 3) 0 3 test-ppid m3ua-ntfy-active-message #f #t #t))
		   peer-addr)
	(sctp-receive-chunk sack-chunk?)
	(list header (+mod32 local-tsn 4) (+mod32 (get-initial-tsn init) 2) peer-addr))
       ((= upper-layer-protocol ulp-diameter)
	(let* ((result (sctp-receive-chunk data-chunk?))
	       (data-chunk (vector-find (cadr result) data-chunk?))
	       (exchange-request (vector->list (get-user-data data-chunk))))
	  (sctp-send header
		     (vector (make-sack-chunk (get-initial-tsn init) 1500 #() #()))
		     peer-addr)
	  (sctp-send header
		     (vector (make-data-chunk (+mod32 local-tsn 0) 0 0
					      diameter-ppid
					      (list->vector (make-capability-exchange-answer-from-request exchange-request
													  diameter-origin-host
													  diameter-origin-realm
													  diameter-host-ip-address
													  diameter-vendor-id
													  diameter-product-name))
					      #f #t #t))
		     peer-addr)
	  (sctp-receive-chunk sack-chunk?))
	(if diameter-sut-sends-initial-watchdog-request
	    (let* ((result (sctp-receive-chunk data-chunk?))
		   (data-chunk (vector-find (cadr result) data-chunk?))
		   (watchdog-request (vector->list (get-user-data data-chunk))))
	      (sctp-send header
			 (vector (make-sack-chunk (+mod32 (get-initial-tsn init) 1) 1500 #() #()))
			 peer-addr)
	      (list header (+mod32 local-tsn 1) (+mod32 (get-initial-tsn init) 2) peer-addr))
	    (list header (+mod32 local-tsn 1) (+mod32 (get-initial-tsn init) 1) peer-addr)))
       (else
	(error "Unsupported upper layer protocol:" upper-layer-protocol))))))

;;; (accept-association)

(define (make-random-bytes length)
  (let ((v (make-vector length)))
    (do ((i 0 (+ i 1)))
	((= i length) v)
      (vector-set! v i (random 256)))))
	
(define (make-ascii-bytes string)
  (list->vector (map char->integer (string->list string))))
