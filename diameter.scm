;;; 
;;; Copyright (c) 2012 Michael Tuexen
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

;;; $Id: diameter.scm,v 1.2 2012/08/27 20:36:57 tuexen Exp $

(define (number-of-padding-bytes l)
  (remainder (- 4 (remainder l 4)) 4))

(define (padding-bytes data)
  (zero-bytes (number-of-padding-bytes (length data))))
;;; (append-padding-bytes (list 1 2 3))

(define mandatory-flag #b01000000)
(define (make-avp code flags data)
  (append (uint32->bytes code)
	  (uint8->bytes flags)
	  (uint24->bytes (+ 8 (length data)))
	  data
	  (padding-bytes data)))

(define siemens-ag-pen 4329)
(define 3gpp-pen 10415)
(define vodafone-information-technology-and-technology-management-pen 12645)
(define nokia-networks-pen 28458)

(define host-ip-address-avp-code 257)
(define auth-application-id-code 258)
(define acct-application-id-code 259)
(define vendor-specific-application-id-code 260)
(define origin-host-avp-code 264)
(define supported-vendor-id-avp-code 265)
(define vendor-id-avp-code 266)
(define result-avp-code 268)
(define product-name-avp-code 269)
(define origin-realm-avp-code 296)

(define (make-host-ip-address-avp address)
  (make-avp host-ip-address-avp-code mandatory-flag (append (list 0 1)
							    (uint32->bytes (inet-pton AF_INET address)))))
;;; (make-host-ip-address-avp "0.0.0.1")

(define (make-auth-application-id-avp id)
  (make-avp auth-application-id-code mandatory-flag (uint32->bytes id)))
;;; (make-auth-application-id-avp 28458)

(define (make-acct-application-id-avp id)
  (make-avp acct-application-id-code mandatory-flag (uint32->bytes id)))
;;; (make-acct-application-id-avp 28458)

(define (make-vendor-specific-application-id-avp vendor-id application-id)
  (make-avp vendor-specific-application-id-code mandatory-flag (append (make-vendor-id-avp vendor-id)
								       (make-auth-application-id-avp application-id))))
;;; (make-vendor-specific-application-id-avp 1 2)

(define (make-origin-host-avp origin-host)
  (make-avp origin-host-avp-code mandatory-flag (string->bytes origin-host)))
;;; (make-origin-host-avp "www1.sctp.de")

(define (make-vendor-id-avp pen)
  (make-avp vendor-id-avp-code mandatory-flag (uint32->bytes pen)))
;;; (make-vendor-id-avp nokia-networks-pen)

(define (make-supported-vendor-id-avp pen)
  (make-avp supported-vendor-id-avp-code mandatory-flag (uint32->bytes pen)))
;;; (make-supported-vendor-id-avp nokia-networks-pen)

(define diameter-success 2001)
(define (make-result-avp result)
  (make-avp result-avp-code mandatory-flag (uint32->bytes result)))
;;; (make-result-avp diameter-success)

(define (make-product-name-avp name)
  (make-avp product-name-avp-code mandatory-flag (string->bytes name)))
;;; (make-product-name-avp "STT")

(define (make-origin-realm-avp origin-realm)
  (make-avp origin-realm-avp-code mandatory-flag (string->bytes origin-realm)))
;;; (make-origin-realm-avp "www1.sctp.de")

(define CER 257) ;;; Capabilities-Exchange-Request
(define CEA 257) ;;; Capabilities-Exchange-Answer
(define DWR 280) ;;; Device-Watchdog-Request
(define DWA 280) ;;; Device-Watchdog-Answer

(define request-flag #b10000000)
(define no-flag 0)
(define version 1)
(define diameter-common-message-app-id 0)

(define (make-diameter-message flags code app-id h2h-id e2e-id avps)
  (append (uint8->bytes version)
	  (uint24->bytes (+ 20 (apply + (map length avps))))
	  (uint8->bytes flags)
	  (uint24->bytes code)
	  (uint32->bytes app-id)
	  (uint32->bytes h2h-id)
	  (uint32->bytes e2e-id)
	  (apply append avps)))
;;; (make-diameter-message request-flag DWR diameter-common-message-app-id 2 3 (list))

(define (get-app-id l)
  (bytes->uint32 (list-tail l 8)))

(define (get-hop-by-hop-id l)
  (bytes->uint32 (list-tail l 12)))
;;; (get-hop-by-hop-id (make-diameter-message request-flag DWR diameter-common-message-app-id 2 3 (list)))

(define (get-end-to-end-id l)
  (bytes->uint32 (list-tail l 16)))
;;; (get-end-to-end-id (make-diameter-message request-flag DWR diameter-common-message-app-id 2 3 (list)))

(define (make-capability-exchange-request app-id h2h-id e2e-id origin-host origin-realm host-ip-address vendor-id product-name)
  (make-diameter-message request-flag CER app-id h2h-id e2e-id
			 (list (make-origin-host-avp origin-host)
			       (make-origin-realm-avp origin-realm)
			       (make-host-ip-address-avp host-ip-address)
			       (make-auth-application-id-avp 16777251)
			       (make-vendor-id-avp vendor-id)
			       (make-product-name-avp product-name))))
;;; (make-capability-exchange-request diameter-common-message-app-id 1 2 "a.b" "c.d" "1.1.1.1" 123 "STT")

(define (make-capability-exchange-answer app-id h2h-id e2e-id result origin-host origin-realm host-ip-address vendor-id product-name)
  (make-diameter-message no-flag CEA app-id h2h-id e2e-id
			 (list (make-result-avp result)
			       (make-origin-host-avp origin-host)
			       (make-origin-realm-avp origin-realm)
			       (make-host-ip-address-avp host-ip-address)
			       (make-auth-application-id-avp 16777251)
			       (make-vendor-id-avp vendor-id)
			       (make-product-name-avp product-name))))
;;; (make-capability-exchange-answer diameter-common-message-app-id 1 2 diameter-success "a.b" "c.d" "1.1.1.1" 123 "STT")

(define (make-capability-exchange-answer-from-request request origin-host origin-realm host-ip-address vendor-id product-name)
  (make-capability-exchange-answer (get-app-id request)
				   (get-hop-by-hop-id request)
				   (get-end-to-end-id request)
				   diameter-success
				   origin-host
				   origin-realm
				   host-ip-address
				   vendor-id
				   product-name))

(define (make-device-watchdog-request app-id h2h-id e2e-id origin-host origin-realm)
  (make-diameter-message request-flag DWR app-id h2h-id e2e-id
			 (list (make-origin-host-avp origin-host)
			       (make-origin-realm-avp origin-realm))))
;;; (make-device-watchdog-request diameter-common-message-app-id 1 2 "a.b" "c.d")

(define (make-device-watchdog-answer app-id h2h-id e2e-id result origin-host origin-realm)
  (make-diameter-message no-flag DWA app-id h2h-id e2e-id
			 (list (make-result-avp result)
			       (make-origin-host-avp origin-host)
			       (make-origin-realm-avp origin-realm))))
;;; (make-device-watchdog-answer diameter-common-message-app-id 1 2 diameter-success "a.b" "c.d")

(define (make-device-watchdog-answer-from-request request origin-host origin-realm)
  (make-device-watchdog-answer (get-app-id request)
			       (get-hop-by-hop-id request)
			       (get-end-to-end-id request)
			       diameter-success
			       origin-host
			       origin-realm))
