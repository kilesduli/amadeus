;;; early-init.el --- amadeus-emacs -*- lexical-binding: t; -*-
;;
;; Author: duli kiles <duli4868@gmail.com>
;; Homepage: https://github.com/kilesduli/amadeus
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(defvar amadeus-init-file (locate-user-emacs-file (format "amadeus-init.%s.el" emacs-version))
  "The quickload file, to load variable and package autoloads etc.")

(unless noninteractive
  (let ((dirs (list "straight" "emacs")))
    (if (seq-every-p (lambda (dir)
                       (file-symlink-p (concat user-emacs-directory dir)))
                     dirs)
        (dolist (dir dirs)
          (set (intern (concat "amadeus-" dir "-directory")) (concat user-emacs-directory dir)))
      (message ":: Run pre-setup first")
      (kill-emacs 1)))

  (if (file-exists-p amadeus-init-file)
      (progn
        (load (concat user-emacs-directory "amadeus-init.el"))
        (amadeus-startup))
    (message ":: No %s, Run pre-setup.el first" (file-name-base amadeus-init-file))
    (kill-emacs 1)))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
