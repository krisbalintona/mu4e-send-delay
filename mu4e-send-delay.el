;;; mu4e-send-delay.el --- Delay sending of mails in mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Benjamin Andresen <benny@in-ulm.de>

;; Author: Benjamin Andresen <benny@in-ulm.de>
;; Maintainer: Benjamin Andresen <benny@in-ulm.de>

;; Version: 20170610.0636
;; URL: https://github.com/jleechpe/outorg-export
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Updated, properly formatted, and cleaned up version of Benjamin Andresen's
;; original version.

;;; Code:

(require 'cl-lib)

(require 'gnus-util)
(autoload 'parse-time-string "parse-time" nil nil)

(require 'mu4e-view)
(require 'mu4e-compose)
(require 'mu4e-draft)

(declare-function org-msg-edit-mode "org-msg" ())

;;;; Custom options
(defgroup mu4e-send-delay nil
  "Customization for delayed sending of messages."
  :group 'mu4e)

(defcustom mu4e-send-delay-header "X-Delay"
  "Header name for storing info about delayed mails."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-strip-header-before-send t
  "If non-nil, remove `mu4e-send-delay-header' before sending mail."
  :type 'bool
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-include-header-in-draft t
  "Whether to include the delay header when starting to draft a message.
If nil, add delay header only when sending the message."
  :type 'bool
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-default-delay "3m"
  "Default length of delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-default-hour "8"
  "Default length of delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-send-delay-timer 120
  "Number of seconds between checks for delayed mail to send."
  :type 'integer
  :group 'mu4e-delay)

;;;; Functions
;;;;; Time strings and parsing
;; Copied from mu4e-delay which is from gnus-delay
(defun mu4e-send-delay-parse-delay-header-string (delay)
  "Return due date for email delayed by or to DELAY.
DELAY is a string, giving the length of the time. Possible values
are:

  ,* <digits><units> for <units> in minutes (`m'), hours (`h'),
    days (`d'), weeks (`w'), months (`M'), or years (`Y');

  ,* YYYY-MM-DD for a specific date. The time of day is given by
    the variable `mu4e-send-delay-default-hour', with the minutes
    and seconds value set to zero.

  ,* hh:mm for a specific time. Use 24h format. If it is later
    than this time, then the deadline is tomorrow, else today."
  (interactive
   (list (read-string
          "Target date (YYYY-MM-DD), time (hh:mm), or length of delay (units in [mhdwMY]): "
          mu4e-send-delay-default-delay)))
  ;; Allow spell checking etc.
  (let (num unit days year month day hour minute deadline)
    (cond ((string-match
            "\\([0-9][0-9][0-9]?[0-9]?\\)-\\([0-9]+\\)-\\([0-9]+\\)"
            delay)
           (setq year  (string-to-number (match-string 1 delay))
                 month (string-to-number (match-string 2 delay))
                 day   (string-to-number (match-string 3 delay)))
           (setq deadline
                 (message-make-date
                  (encode-time 0 0      ; second and minute
                               mu4e-send-delay-default-hour
                               day month year))))
          ((string-match "\\([0-9]+\\):\\([0-9]+\\)" delay)
           (setq hour   (string-to-number (match-string 1 delay))
                 minute (string-to-number (match-string 2 delay)))
           ;; Use current time, except...
           (setq deadline (apply 'vector (decode-time (current-time))))
           ;; ... for minute and hour.
           (aset deadline 1 minute)
           (aset deadline 2 hour)
           ;; Convert to seconds.
           (setq deadline (float-time (apply 'encode-time
                                             (append deadline nil))))
           ;; If this time has passed already, add a day.
           (when (< deadline (float-time))
             (setq deadline (+ 86400 deadline))) ; 86400 secs/day
           ;; Convert seconds to date header.
           (setq deadline (message-make-date
                           (seconds-to-time deadline))))
          ((string-match "\\([0-9]+\\)\\s-*\\([mhdwMY]\\)" delay)
           (setq num (match-string 1 delay))
           (setq unit (match-string 2 delay))
           ;; Start from seconds, then multiply into needed units.
           (setq num (string-to-number num))
           (cond ((string= unit "Y")
                  (setq delay (* num 60 60 24 365)))
                 ((string= unit "M")
                  (setq delay (* num 60 60 24 30)))
                 ((string= unit "w")
                  (setq delay (* num 60 60 24 7)))
                 ((string= unit "d")
                  (setq delay (* num 60 60 24)))
                 ((string= unit "h")
                  (setq delay (* num 60 60)))
                 (t
                  (setq delay (* num 60))))
           (setq deadline (message-make-date
                           (seconds-to-time (+ (float-time) delay)))))
          (t (error "Malformed delay `%s'" delay)))
    deadline))

(defun mu4e-send-delay-return-delay-header-value (file-path)
  "Return delay header value for email at FILE-PATH."
  (if (file-exists-p file-path)
      (let (found-value parsed-value)
        (with-temp-buffer
          (insert-file-contents file-path)
          (setq found-value (message-fetch-field mu4e-send-delay-header))
          (when found-value
            (setq parsed-value (mu4e-send-delay-parse-delay-header-string found-value))
            ;; Make sure found-value is a parsed time string (a little janky,
            ;; but there is no real-world use case this would fail)
            (when (eq (length found-value) (length parsed-value))
              found-value))))
    (user-error "[mu4e-send-delay-return-delay-header-value] %s does not exist" file-path)))

(defun mu4e-send-delay-elapsed-p (file-path)
  "Return non-nil if the time string in email at FILE-PATH has passed."
  (when-let* ((header-value (mu4e-send-delay-return-delay-header-value file-path))
              (parsed-ts (parse-time-string header-value)))
    (unless (cl-every #'null parsed-ts)
      (let* ((delay-time (encode-time parsed-ts))
             (time-passed (time-since delay-time)))
        (and (>= (nth 0 time-passed) 0)
             (>= (nth 1 time-passed) 0))))))

;;;;; Scheduling during email composition
(defun mu4e-send-delay-schedule-and-exit ()
  "Schedule send email and exit current email composition buffer."
  (condition-case err
      (let* ((schedule-time
              (mu4e-send-delay-parse-delay-header-string (or (message-fetch-field mu4e-send-delay-header) mu4e-send-delay-default-delay))))
        ;; Replace delay header value with a time string
        (message-remove-header mu4e-send-delay-header nil)
        (message-add-header (format "%s: %s" mu4e-send-delay-header schedule-time))

        (when (buffer-file-name) (mu4e~compose-set-parent-flag (buffer-file-name))) ; Set reply/forward flag

        (message-dont-send)
        (when message-kill-buffer-on-exit (kill-buffer (current-buffer)))
        (mu4e-message "Mail scheduled to send at %s" schedule-time))
    (error (princ (format "mu4e-send-delay: %s" err)))))

(defun mu4e-send-delay-send-and-exit (&optional delay)
  "Send this email.

If DELAY, then delay sending this email."
  (interactive "P")
  (run-hooks 'message-send-hook)
  (if delay
      (mu4e-send-delay-schedule-and-exit)
    (when mu4e-send-delay-strip-header-before-send
      (message-remove-header mu4e-send-delay-header nil))
    (message-send-and-exit))) ; REVIEW 2023-07-08: Should this change if using `org-msg?'

;;;;; Sending
(defmacro mu4e-send-delay-with-mu4e-context (context &rest body)
  "Evaluate BODY under CONTEXT.

Sets `mu4e--context-current' to CONTEXT and evaluates with
`with-mu4e-context-vars'."
  (declare (indent 2))
  `(let* ((mu4e--context-current ,context))
     (with-mu4e-context-vars ,context
         ,@body)))

(defun mu4e-send-delay-move-or-delete-draft (file-path)
  "Move mail at FILE-PATH appropriately or delete stored draft.

Be aware that `mu4e-sent-messages-behavior' should be set to
`trash' or `delete' if using GMail, since GMail automatically
sends copies to the sent folder, meaning a value of `set' will
lead to duplicate emails that you will have to manually remove."
  ;; NOTE 2023-07-08: The FCC header is only added by mu4e for the values of
  ;; `trash' and `send'. See `mu4e~compose-setup-fcc-maybe'
  (pcase mu4e-sent-messages-behavior
    ((or 'sent 'trash)
     ;; REVIEW 2023-07-08: Not sure if this is the correct behavior, since I
     ;; don't use `trash' or `sent', so I haven't tested it
     (with-temp-buffer
       (insert-file-contents file-path)
       (when-let ((file (message-fetch-field "fcc")))
         (message-remove-header "fcc" nil)
         (message-remove-header mu4e-send-delay-header nil)
         (write-file file)
         (set-buffer-modified-p nil))
       (delete-file file-path)))
    ('delete (delete-file file-path))))

(defun mu4e-send-delay-send-if-due (file-path)
  "Send mail at FILE-PATH if it should be sent and is not currently open."
  (when (and (mu4e-send-delay-elapsed-p file-path)
             (not (get-file-buffer file-path))) ; Not opened
    (condition-case err
        (progn
          (with-current-buffer (find-file-noselect file-path)
            ;; Force recode to fix character encoding issue
            (set-buffer-file-coding-system 'utf-8 t)
            (recode-region (point-min) (point-max) 'prefer-utf-8 'utf-8-unix)
            (mu4e~draft-insert-mail-header-separator)
            (message-mode)
            (when mu4e-send-delay-strip-header-before-send
              (message-remove-header mu4e-send-delay-header nil))
            (message-send))
          ;; Then deal with the original file (by moving it to the appropriate
          ;; folder or deleting it)
          (mu4e-send-delay-move-or-delete-draft file-path)
          t)
      (error "mu4e-delay-send: %s" err))))

(defun mu4e-send-delay-send-due ()
  "Send all delayed mail drafts that are due."
  (interactive)
  (when (mu4e-root-maildir)
    (let* ((dirs (if mu4e-contexts
                     (mapcar (lambda (context)
                               (mu4e-send-delay-with-mu4e-context context
                                   (expand-file-name "cur" (concat (mu4e-root-maildir) (mu4e-get-drafts-folder)))))
                             mu4e-contexts)
                   (list (expand-file-name "cur" (concat (mu4e-root-maildir) (mu4e-get-drafts-folder)))))))
      (when (memq t
                  (mapcar (lambda (dir)
                            (cl-loop for file in (directory-files dir t "^[^\.]")
                                     collect (mu4e-send-delay-send-if-due file)))
                          dirs))
        ;; Only update index if something was done
        (mu4e-update-index)))))

;;;;; Timer
(defvar mu4e-send-delay-send-due-timer nil
  "Timer to run `mu4e-send-delay-send-due'.")

(defun mu4e-send-delay-initialize-send-queue-timer ()
  "Set up `mu4e-send-delay-send-due' to run on a timer."
  (interactive)
  (unless mu4e-send-delay-send-due-timer
    (setq mu4e-send-delay-send-due-timer
          (run-with-timer 0 mu4e-send-delay-timer 'mu4e-send-delay-send-due))))

;;;; Setup
(defun mu4e-send-delay-setup ()
  "Make sure delay header is added when composing emails.

Advise `mu4e~draft-common-construct' since it is used by mu4e to
insert headers across all mu4e's email composition buffers.

Running this command more than once will advise
`mu4e~draft-common-construct' multiple times, leading to multiple
delay headers being inserted upon composition (which can be
manually removed afterward)."
  (interactive)
  (mu4e-send-delay-initialize-send-queue-timer)
  (advice-add 'mu4e~draft-common-construct :around
                                           #'(lambda (orig-fun &rest args)
                                                (concat
                                                 (apply orig-fun args)
                                                 (when mu4e-send-delay-include-header-in-draft
                                                   (mu4e~draft-header mu4e-send-delay-header mu4e-send-delay-default-delay))))))

;; Show in the main view
(add-to-list 'mu4e-header-info-custom
             '(:send-delay . (:name "X-Delay"
                              :shortname "Delay"
                              :help "Date/time when mail is scheduled for sending"
                              :function (lambda (msg)
                                          (or
                                           (mu4e-send-delay-return-delay-header-value
                                            (mu4e-message-field msg :path))
                                           "")))))
(add-to-list 'mu4e-view-fields :send-delay t)

;;;

(provide 'mu4e-send-delay)

;;; mu4e-send-delay.el ends here
