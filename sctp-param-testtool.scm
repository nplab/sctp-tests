;;;
;;; Copyright (c) 2003-2012 Michael Tuexen
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

;;; $Id: sctp-param-testtool.scm,v 1.13 2014/11/17 14:55:29 tuexen Exp $

(define sut-addr (make-ipv4-address "212.201.121.105"))
(define sut-port 80)

;;; valid values are #t (true) or #f (false)
(define sut-is-server #t)

(define sut-maximum-init-retransmits 8)
(define sut-maximum-assoc-retransmits 10)

;;; valid values are ulp-diameter, ulp-echo, ulp-http, ulp-m3ua, or ulp-s1ap
;;; For SGsAP use ulp-echo
(define upper-layer-protocol ulp-http)

;;; When using different addresses for tester-addr-1 and
;;; tester-addr-2, connection setups using dual-homing will be used.
;;; When using the same address for tester-addr-1 and
;;; tester-addr-2, connection setups using single-homing will be used.
;;; tester-addr-3 must be different from tester-addr-1 and tester-addr-2.
(define tester-port 5001)
(define tester-addr-1 (make-ipv4-address "217.249.127.211"))
(define tester-addr-2 (make-ipv4-address "217.249.127.211"))
(define tester-addr-3 (make-ipv4-address "192.0.2.1"))

;;; valid values are heartbeat-mode-discard, heartbeat-mode-confirm, heartbeat-mode-reflect
(define tester-heartbeat-mode heartbeat-mode-reflect)

(define tester-os  2)
(define tester-mis 2)

;;; tester-handshake-wait is a delay before progressing after
;;; an inital handshake
(define tester-handshake-wait 0)

;;; tester-long-wait needs to be long enough that an
;;; association dies and a COOKIE is not valid anymore.
(define tester-long-wait 65)
(define tester-short-wait 3)

(define diameter-h2h-id 1)
(define diameter-e2e-id 2)
(define diameter-origin-host "mba.testbed")
(define diameter-origin-realm "testbed")
(define diameter-host-ip-address "192.168.115.132")
(define diameter-vendor-id 1)
(define diameter-product-name "stt")
(define diameter-cer-avps (list (make-supported-vendor-id-avp 3gpp-pen)
				(make-supported-vendor-id-avp vodafone-information-technology-and-technology-management-pen)
				(make-supported-vendor-id-avp siemens-ag-pen)
				(make-supported-vendor-id-avp nokia-networks-pen)
				(make-vendor-specific-application-id-avp 3gpp-pen 3gpp-zh-app-id)
				(make-vendor-specific-application-id-avp 3gpp-pen 3gpp-swx-app-id)
				(make-vendor-specific-application-id-avp 3gpp-pen 3gpp-s6a-app-id)
				(make-vendor-specific-application-id-avp 3gpp-pen 3gpp-sh-app-id)
				(make-vendor-specific-application-id-avp 3gpp-pen 3gpp-wx-app-id)
				(make-vendor-specific-application-id-avp nokia-networks-pen nsn-hd-application-app-id)))

(define diameter-sut-sends-initial-watchdog-request #t)

(define diameter-watchdog-request (list->vector (make-device-watchdog-request diameter-common-message-app-id
									      diameter-h2h-id
									      diameter-e2e-id
									      diameter-origin-host
									      diameter-origin-realm)))

(define diameter-capability-exchange-request (list->vector (make-capability-exchange-request diameter-common-message-app-id
											     diameter-h2h-id
											     diameter-e2e-id
											     diameter-origin-host
											     diameter-origin-realm
											     diameter-host-ip-address
											     diameter-vendor-id
											     diameter-product-name
											     diameter-cer-avps)))

(define (make-echo-test-message length total-length)
  (make-random-bytes length))

(define m3ua-beat-message (list->vector (list 1 0 3 3 0 0 0 24 0 9 0 14 77 51 85 65 32 114 111 99 107 115 0 0)))
(define m3ua-aspup-message (list->vector (list 1 0 3 1 0 0 0 24 0 4 0 14 77 51 85 65 32 114 111 99 107 115 0 0)))
(define m3ua-aspup-ack-message (list->vector (list 1 0 3 4 0 0 0 8)))
(define m3ua-aspac-ack-message (list->vector (list 1 0 4 3 0 0 0 8)))
(define m3ua-ntfy-inactive-message (list->vector (list 1 0 0 1 0 0 0 16 0 13 0 8 0 1 0 2)))
(define m3ua-ntfy-active-message  (list->vector (list 1 0 0 1 0 0 0 16 0 13 0 8 0 1 0 3)))
;;;(define m3ua-aspac-ack-message (list->vector (list 1 0 4 3 0 0 0 24 0 11 0 8 0 0 0 2 0 6 0 8 0 0 0 1)))
(define (make-m3ua-test-message length total-length)
  (list->vector (apply append (list (list 1 0 3 3)
				    (list 0 0 (quotient total-length 256) (remainder total-length 256))
				    (list 0 9 (quotient (- total-length 8) 256) (remainder (- total-length 8) 256))
				    (vector->list (make-random-bytes (- length 12)))))))

(define http-test-message (make-ascii-bytes "GET /index.html HTTP/1.0\r\nUser-agent: stt\r\nConnection: close\r\n\r\n"))

(define heartbeat-chunk (make-heartbeat-chunk (make-heartbeat-parameter (make-ascii-bytes "SCTP rocks."))))

(define test-ppid 0)
(define test-message '())
(define make-test-message (lambda (length total-length)
			    (error "Can't generate test message of total length:" total-length)))
(cond
 ((= upper-layer-protocol ulp-echo)
  (set! test-ppid echo-ppid)
  (set! test-message (make-ascii-bytes "SCTP rocks."))
  (set! make-test-message make-echo-test-message))
 ((= upper-layer-protocol ulp-http)
  (set! test-ppid http-ppid)
  (set! test-message http-test-message))
;;;  (set! make-test-message make-http-test-message))
 ((= upper-layer-protocol ulp-m3ua)
  (set! test-ppid m3ua-ppid)
  (set! test-message m3ua-beat-message)
  (set! make-test-message make-m3ua-test-message))
 ((= upper-layer-protocol ulp-diameter)
  (set! test-ppid diameter-ppid)
  (set! test-message diameter-watchdog-request))
 ((= upper-layer-protocol ulp-s1ap)
  (set! test-ppid s1ap-ppid)
  ;;; This is a reset message
  (set! test-message (vector #x00 #x0e #x00 #x0d #x00 #x00 #x02 #x00 #x02 #x40 #x01 #x44 #x00 #x5c #x00 #x01 #x00)))
 (else
  (error "Unsupported upper layer protocol:" upper-layer-protocol)))
