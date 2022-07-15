;;; elisp-flymake-form.el --- Configure flymake form -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-flymake-form
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; Ensure that all top level forms in buffer are lists.

;; Commands

;; M-x `elisp-flymake-form-setup'
;;      Add `elisp-flymake-form-lint' into `flymake-diagnostic-functions' and turn on flymake.

;;; Code:


(eval-when-compile
  (require 'cl-lib))

(require 'flymake)

(declare-function flymake-diag-region "flymake")
(declare-function flymake-make-diagnostic "flymake")

(defun elisp-flymake-form-re-search-backward-inner (regexp &optional
                                                           bound count)
  "This function is helper for `elisp-flymake-form-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-flymake-form-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `elisp-flymake-form-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun elisp-flymake-form-re-search-forward (regexp &optional
                                                    bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'elisp-flymake-form-re-search-backward-inner)
               ((> count 0) #'elisp-flymake-form-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun elisp-flymake-form-re-search-backward (regexp &optional
                                                     bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (elisp-flymake-form-re-search-forward regexp bound noerror
                                        (if count
                                            (- count) -1)))

(defun elisp-flymake-form-scan-dubs ()
  "Check current buffer for dublicate definitions and return list of links."
  (save-excursion
    (let ((dubs)
          (all '()))
      (goto-char (point-max))
      (while (let ((pos (point))
                   (end))
               (setq end (ignore-errors
                           (backward-list 1)
                           (point)))
               (unless (equal pos end)
                 end))
        (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                            (list-at-point)))
                    (id (nth 1 sexp))
                    (type (car sexp))
                    (line (line-number-at-pos))
                    (cell (list id type line)))
          (when-let ((dub (seq-find (lambda (it) (and (eq (car it) id)
                                                 (eq (nth 1 it) type)))
                                    all)))
            (push `(,(nth 2 dub) 0 error
                    ,(format "%s `%s' already defined" type id))
                  dubs)
            (push `(,line 0 error
                          ,(format "%s `%s' already defined" type id))
                  dubs))
          (push cell all)))
      dubs)))

(defun elisp-flymake-form-lint-buffer ()
  "Check current elisp buffer for typos and return list of links."
  (save-excursion
    (goto-char (point-max))
    (let ((problems))
      (while (elisp-flymake-form-re-search-backward "[^\s\t\n\f]" nil t 1)
        (cond ((looking-at "[)]")
               (forward-char 1)
               (forward-sexp -1))
              (t
               (let ((col 0)
                     (line (line-number-at-pos)))
                 (skip-chars-backward "^\s\t\n\f")
                 (setq col (current-column))
                 (push `(,line ,col error "Unexpected char") problems)))))
      problems)))

(defun elisp-flymake-form-lint (report-fn &rest _args)
  "A Flymake backend that check that all top level elisp forms are lists.
Use `elisp-flymake-form-setup' to add this to
`flymake-diagnostic-functions'.  Calls REPORT-FN directly."
  (let ((collection (elisp-flymake-form-lint-buffer)))
    (cl-loop for (line col type message) in
             collection
             for (beg . end) = (flymake-diag-region (current-buffer) line col)
             collect
             (flymake-make-diagnostic
              (current-buffer)
              beg end
              (if (eq type 'warning) :warning :error)
              message)
             into diags
             finally (funcall report-fn diags)))
  (funcall report-fn nil))

;;;###autoload
(defun elisp-flymake-form-setup ()
  "Add `elisp-flymake-form-lint' into `flymake-diagnostic-functions'."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'elisp-flymake-form-lint nil t)
  (flymake-mode))

(provide 'elisp-flymake-form)
;;; elisp-flymake-form.el ends here