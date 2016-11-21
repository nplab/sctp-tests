;;; 
;;; Copyright (C) 2014 M. Tuexen tuexen@fh-muenster.de
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

;;; $Id: sctp-checksum-tests.scm,v 1.2 2014/07/16 21:17:44 tuexen Exp $

(define (sctp-checksum-crc32c-with-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-crc32c (make-common-header local-port peer-port (choose-tag))
			 (vector (make-shutdown-ack-chunk))
			 peer-addr)
  (let ((result (sctp-receive 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-failed
	(let ((chunk (vector-ref (cadr result) 0)))
	  (if (and (shutdown-complete-chunk? chunk)
		   (equal? (get-t-bit chunk) #t))
	      stt-test-result-passed
	      stt-test-result-failed)))))

(define (sctp-checksum-adler32-with-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-adler32 (make-common-header local-port peer-port (choose-tag))
			  (vector (make-shutdown-ack-chunk))
			  peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout shutdown-complete-chunk? 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))

(define (sctp-checksum-zero-with-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-zero (make-common-header local-port peer-port (choose-tag))
		       (vector (make-shutdown-ack-chunk))
		       peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout shutdown-complete-chunk? 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))

(define (sctp-checksum-wrong-with-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-wrong (make-common-header local-port peer-port (choose-tag))
			(vector (make-shutdown-ack-chunk))
			peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout shutdown-complete-chunk? 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))

(define (sctp-checksum-crc32c-without-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-crc32c (make-common-header local-port peer-port 0)
			 (vector (make-init-chunk local-tag 1500 tester-os tester-mis (random (expt 2 32)) (if (equal? tester-addr-1 tester-addr-2)
													       (vector)
													       (vector (make-ipv4-address-parameter tester-addr-1)
														       (make-ipv4-address-parameter tester-addr-2)))))
			 peer-addr)
  (let ((result (sctp-receive 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-failed
	(let ((header (car result))
	      (chunk (vector-ref (cadr result) 0)))
	  (if (and (= (get-verification-tag header) local-tag)
		   (or (init-ack-chunk? chunk)
		       (and (abort-chunk? chunk) (equal? (get-t-bit chunk) #f))))
	      stt-test-result-passed
	      stt-test-result-failed)))))

(define (sctp-checksum-adler32-without-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-adler32 (make-common-header local-port peer-port 0)
			  (vector (make-init-chunk local-tag 1500 tester-os tester-mis (random (expt 2 32)) (if (equal? tester-addr-1 tester-addr-2)
														(vector)
														(vector (make-ipv4-address-parameter tester-addr-1)
															(make-ipv4-address-parameter tester-addr-2)))))
			  peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout (lambda (c) (or (init-ack-chunk? c)
								 (abort-chunk? c)))
						 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))

(define (sctp-checksum-zero-without-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-zero (make-common-header local-port peer-port 0)
		       (vector (make-init-chunk local-tag 1500 tester-os tester-mis (random (expt 2 32)) (if (equal? tester-addr-1 tester-addr-2)
													     (vector)
													     (vector (make-ipv4-address-parameter tester-addr-1)
														     (make-ipv4-address-parameter tester-addr-2)))))
		       peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout (lambda (c) (or (init-ack-chunk? c)
								 (abort-chunk? c)))
						 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))

(define (sctp-checksum-wrong-without-padding peer-server? peer-addr local-port peer-port)
  (sctp-cleanup)
  (sctp-send-with-wrong (make-common-header local-port peer-port 0)
			(vector (make-init-chunk local-tag 1500 tester-os tester-mis (random (expt 2 32)) (if (equal? tester-addr-1 tester-addr-2)
													      (vector)
													      (vector (make-ipv4-address-parameter tester-addr-1)
														      (make-ipv4-address-parameter tester-addr-2)))))
			peer-addr)
  (let ((result (sctp-receive-chunk-with-timeout (lambda (c) (or (init-ack-chunk? c)
								 (abort-chunk? c)))
						 1000)))
    (if (equal? result (list #f #f #f #f #f))
	stt-test-result-passed
	stt-test-result-failed)))
