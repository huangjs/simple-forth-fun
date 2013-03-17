;;; keys: global
(require 'paredit)
(paredit-mode 1)
(define-key paredit-mode-map (kbd ")") 'paredit-close-parenthesis)
(define-key paredit-mode-map (kbd "C-k") 'kill-sexp)
(define-key paredit-mode-map (kbd "M-k") 'paredit-kill) 
(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-sexp)
(define-key paredit-mode-map (kbd "M-)") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-[") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-)") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-d") 'delete-char)
(define-key paredit-mode-map (kbd "C-M-j") 'paredit-forward-down)
;; clear
(define-key paredit-mode-map (kbd "M-r") nil)
(define-key paredit-mode-map (kbd "M-s") nil) 

