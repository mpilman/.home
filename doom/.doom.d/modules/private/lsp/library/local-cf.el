;;; local-cf.el --- Only reformat changed lines    -*- lexical-binding: t; -*-

;; this file is not part of emacs

;; Copyright (C) 2018 Markus Pilman
;; Author: Markus Pilman, Le Wang
;; Maintainer: Markus Pilman
;; Description: Only reformat changed lines

;;; Commentary:

;; A lot of code to keep track of changes is taken from the
;; ws-butler plugin by Le Wang.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defvar local-cf-hook nil
  "The hook to run when a local-cf detects a change")

(defun local-cf-after-change (beg end length-before)
  (let ((type (if (and (= beg end) (> length-before 0))
                  'delete
                'chg)))
    (if undo-in-progress
        ;; add back deleted text during undo
        (if (and (zerop length-before)
                 (> end beg)
                 (eq (get-text-property end 'local-cf-chg) 'delete))
            (remove-list-of-text-properties end (1+ end) '(local-cf-chg)))
      (with-silent-modifications
        (when (eq type 'delete)
          (setq end (min (+ end 1) (point-max))))
        (put-text-property beg end 'local-cf-chg type)))))

(defun local-cf-map-changes (func &optional start-pos end-pos)
    "Call FUNC with each position"
    (let ((start (or start-pos (point-min)))
          (limit (copy-marker (or end-pos (point-max))))
          prop end)
      (while (and start (< start limit))
        (setq prop (get-text-property start 'local-cf-chg))
        (setq end (text-property-not-all start limit 'local-cf-chg prop))
        (if prop
            (funcall func prop start (or end limit)))
          (setq start end))
        (set-marker limit nil)))

(defun local-cf-before-save ()
  "Run local-cf-hook on every changed region"
  (let (last-end)
    (local-cf-map-changes
     (let ((curr-begin nil)
           (curr-end (point-min)))
       (lambda (_prop start end)
         (if (not curr-begin)
             (progn
               (setq curr-begin start)
               (setq curr-end end))
           (if (<= end curr-end)
               (setq curr-end end)
             (progn
               (run-hook-with-args 'local-cf-hook curr-begin curr-end)
               (setq curr-begin nil)))
           ))
       ; we still need to call the hooks with the last range
       (if curr-begin
           (run-hook-with-args 'local-cf-hook curr-begin curr-end))))))

(defun local-cf-clear-properties ()
  "Clear all properties we set during edits"
  (with-silent-modifications
    (local-cf-map-changes
     (lambda (_prop start end)
       (remove-list-of-text-properties start end '(local-cf-chg))))))

;;;###autoload
(define-minor-mode local-cf-mode
  "Run format only on changed lines"
  :lighter " lcf"
  :group 'local-cf
  (if local-cf-mode
      (progn
        (add-hook 'after-change-functions 'local-cf-after-change t t)
        (add-hook 'before-save-hook 'local-cf-before-save t t)
        (add-hook 'edit-server-done-hook 'local-cf-before-save t t)
        (add-hook 'after-save-hook 'local-cf-clear-properties t t)
        (add-hook 'after-revert-hook 'local-cf-clear-properties t t))
  (remove-hook 'after-change-functions 'local-cf-after-change t)
  (remove-hook 'before-save-hook 'local-cf-before-save t)
  (remove-hook 'edit-server-done-hook 'local-cf-before-save t)
  (remove-hook 'after-save-hook 'local-cf-clear-properties t)
  (remove-hook 'after-revert-hook 'local-cf-clear-properties t)))

(provide 'local-cf)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local-cf.el ends here
