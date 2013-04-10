;;; flymake-python.el --- Flymake support for Python
;; 
;; Author: Sean Fisk
;; Maintainer: Sean Fisk
;; Keywords: languages, local, processes, tools
;; Compatibility: GNU Emacs: 23.x, Aquamacs: 2.x
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
;; 
;;; Code:

(defconst flymake-allowed-python-file-name-masks '(("\\.py\\'" flymake-python-init)))

;; syntax checker for Python - examples include `pep8', `pyflakes', `flake8' (combination of pep8, pyflakes, and McCabe), `pychecker', or a custom shell script
(defcustom flymake-python-syntax-checker "pyflakes"
  "Syntax checker for Flymake Python."
  :type 'string)

(defun flymake-python-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-python")))

(defun flymake-python-init ()
  (list flymake-python-syntax-checker (list (flymake-init-create-temp-buffer-copy
					     'flymake-python-create-temp-in-system-tempdir))))

(defun flymake-python-load ()
  (interactive)
  (require 'flymake)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-allowed-python-file-name-masks)
  (if (executable-find flymake-python-syntax-checker)
      (flymake-mode t)
    (message (concat "Not enabling flymake: " flymake-python-syntax-checker " command not found"))))

(add-hook 'python-mode-hook 'flymake-python-load)

(provide 'flymake-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-python.el ends here
