;;; early-init.el --- amadeus-emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(let ((dirs (list "straight" "emacs")))
  (if (seq-every-p (lambda (dir)
                     (file-symlink-p (concat user-emacs-directory dir)))
                   dirs)
      (dolist (dir dirs)
        (set (intern (concat "amadeus-" dir "-directory")) (concat user-emacs-directory dir)))
    (message ":: Run pre-setup.el first")
    (kill-emacs 1)))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
