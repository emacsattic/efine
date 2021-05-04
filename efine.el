;;; efine.el ---  Interface to FinnPlace online English-Finnish-English dictionary

;; Copyright (C) 2001 Sami Salkosuo
;; Author: Sami Salkosuo 
;; Version: 0.1 Fri Oct 12 16:23:20 2001

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is EMACS interface to online English-Finnish-English dictionary.
;; See URL: EnfinCD dictionary demo page http://212.213.217.194/cd/enfin4.htm"
;;
;; Installation:
;;
;; Add efine.el to your load path and add
;; (require 'efine)
;; to .emacs
;;
;; If using efine from behind proxy
;; (setq efine-proxy-host )
;; (setq efine-proxy-port )
;;
;; Usage:
;; M-x efine and type english or finnish word for translation
;; 
;; Known problems:
;;
;; Umlauts (�/�) could cause incorrect results.

;;; Code:

(defvar efine-proxy-host nil
  "HTTP proxy host")

(defvar efine-proxy-port nil
  "HTTP proxy port")

(defvar efine-enfin-host "212.213.217.194"
  "FinnPlace Dictionary host")

(defvar efine-enfin-file "cgi-bin/mofind.exe/dr1?word="
  "FinnPlace Dictionary port")

(defvar efine-word ""
  "Word to be translated")

(defun efine (word)
  "Interface to Efine dictionary.
   URL: EnfinCD dictionary demo page http://212.213.217.194/cd/enfin4.htm"
  (interactive "sWord: ")  
  (let (
	(host)
	(port 80)
	(buffer)
	(tcp-connection)
	(request)
	)
    ;;set proxy if needed
    (if efine-proxy-host
	(progn
	  (setq file (concat "http://" efine-enfin-host "/" efine-enfin-file))
	  (setq host efine-proxy-host)
	  (setq port efine-proxy-port)
	  )
      (progn
	(setq host efine-enfin-host)
	(setq file (concat "/" file))
	)
      )
    (setq buffer (get-buffer-create "*E-Fin-E*"))
    (set-buffer buffer)
    (erase-buffer)
    (goto-char 0)

    (setq tcp-connection
	  (open-network-stream
	   "GET efine"
	   buffer
	   host
	   port
	   ))
    ;(process-status tcp-connection)
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-sentinel tcp-connection 'efine-sentinel)
    
    (setq efine-word word)
    ;;url encode
    (setq word (string-replace word "�" "%e4"))
    (setq word (string-replace word "�" "%f6"))

    (setq request (concat "GET " file word " HTTP/1.0\n\n"))
    (process-send-string tcp-connection request)
    ;;(accept-process-output)

    ;;parse html
    (efine-parse tcp-connection)
    (kill-buffer buffer)
    (delete-process tcp-connection)
    )
  )

(defun efine-parse (process)
  "Parses results"
  (let (
	(word)
	(buffer)
	(buffer-text)
	(new-point)
	(table-start)
	(table-end)
	(tag-start)
	(tag-end)
	(word-list)	
	(word-list2)
	(temp)
	(translation)
	)
    (while (eq (process-status process) 'open)
      (sit-for 0 200)
      )
    (setq word-list nil)
    (setq translation "")
    ;;find 'word'
    ;;find table start tag
    ;;for each row
    ;;  column 1: translation
    ;;  column 2:
    (setq word efine-word)
    (setq buffer (get-buffer-create "*E-Fin-E*"))
    ;;(set-buffer buffer)    
    (goto-char 0)
    ;;check results
    (if (re-search-forward "[nN]o words found" nil t)
	(progn
	  (message (concat "No words found. Word: " word))	  
	  )
      (progn
	;;check umlauts
	(if (string-match "[��]" word)
	    (progn
	      (setq temp (string-replace word "�" ".?"))
	      (setq temp (string-replace temp "�" ".?"))
	      (setq new-point (re-search-forward temp nil t))
	      )
	  (setq new-point (search-forward word nil t))
	  )
	(if new-point
	    (progn
	      (delete-region 1 new-point)
	      (downcase-region 1 (point-max))    
	      (setq table-start (search-forward "" nil t))
	      (while tag-start
		(setq tag-end (search-forward "" nil t))
		;;(setq temp (concat temp (number-to-string tr-start) ":" (number-to-string tr-end) "\n"))
		(setq word-list (append word-list (list (list (buffer-substring tag-start tag-end))) nil))
		(setq tag-start (search-forward "" nil t))
		)
	      ;;now we have list of table rows from dictionary
	      (while word-list
		(setq temp (car word-list))
		;;remove all tags
		(erase-buffer)
		(insert (car temp))
		(goto-char 0)	    
		(while (re-search-forward "<+[a-zA-Z0-9# =\x2b/]*>+" nil t);;\2b= '+'
		  (replace-match "")
		  ) 
		(goto-char 0)
		(replace-string "\n" " ")
		(setq word-list2 (split-string (buffer-string)))
		(while word-list2
		  (setq temp (car word-list2))
		  (setq translation (concat translation temp " "))
		  (setq word-list2 (cdr word-list2))
		  )
		(setq translation (concat translation "\n"))
		;;(setq tag-start (re-search-forward "<+[a-zA-Z]*>+" nil t))
		(setq word-list (cdr word-list))
		)


	      ;;todo prettify results

	      (setq translation (concat "Results from FinnPlace Dictionary\nWord: " word "\n\n" translation))
	      (with-electric-help
	       '(lambda () (insert translation) (goto-char 0) "*E-Fin-E*")
	       )
	      )
	  (progn
	    (message (concat "Failed. Word: " word))
	    )
	  )
	)
      )
    )
  )


(defun efine-sentinel (process string)
  "Process the results from the efine network connection.
process - The process object that is being notified.
string - The string that describes the notification."
  )

(defun string-replace (body to-string from-string)
  "Replace to-string to from-string in body"
  (let (
	(index)
	(modified-string)
	)
    (setq modified-string body)
    (setq index (string-match to-string modified-string))
    (while index
      (setq modified-string (concat (substring modified-string 0 index) from-string (substring modified-string (+ index (length to-string))) ))
      (setq index (string-match to-string modified-string))
      )
    modified-string
    )
  )

(provide 'efine)
