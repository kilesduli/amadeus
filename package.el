;;; package.el --- straight package file -*- lexical-binding: t; -*-
;;
;; Author: duli kiles <duli4868@gmail.com>
;; Homepage: https://github.com/kilesduli/amadeus
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:
(require 'cl-lib)

;; emacs script do not load early-init.el, we still disable it.
(setq package-enable-at-startup nil)
(setq straight-repository-branch "develop"
      ;; We do not use use-package
      straight-use-package-by-default nil
      ;; Separate from other builds
      straight-build-dir (format "amadeus-build-%s" emacs-version)
      ;; just depth 1
      straight-vc-git-default-clone-depth '(1 single-branch))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(cl-defmacro package! (package &key recipe)
  "Declare all packages and use `straight-use-package'."
  (declare (indent defun))
  `(straight-use-package
    ,(if recipe
         `(quote ,(cons package recipe))
       `(quote ,package))))

;;;; Package declaration


;;; package.el ends here
