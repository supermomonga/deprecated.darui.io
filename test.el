
(progn
  (require 'server)
  (setq server-host "0.0.0.0")
  (setq server-port "1234")
  (setq server-use-tcp t)
  (setq server-name "emacs_server")
  (let ((file-dir
         (mapconcat 'identity
                    (append
                     (reverse (cdr (reverse (split-string (buffer-file-name) "/"))))
                     '("serverd"))
                    "/")))
    (setq server-auth-dir file-dir)))

(server-eval-at "emacs_server" '(+ 1 10))

(server-running-p)
