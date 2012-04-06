;;; ssh-config.el --- Manage both ssh and dsh confguration from emacs

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, ssh
;; Created: 2010-11-22
;; Last changed: 2012-04-07 00:03:34
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; Add support for multipath SSH command:
;;  - Org property: :Other-Path: jh1
;;  - Result in ssh config:
;;    ProxyCommand sh -c "ssh -q -t PROXY nc -w 60 %h %p || ssh -q -t jh1 nc -w 60 %h %p"
;;
;; If PROXY could not be joined, try to connect via jh1.

;;; Code:

(eval-when-compile
  (require 'cl nil t)
  (require 'vc-hooks nil t)
  (require 'files nil t))
(require 'org)

(defcustom sc:ssh-config-file "~/.ssh/config"
  "Path to user ssh configuration file."
  :group 'ssh-config
  :type 'string)

(defcustom sc:dsh-config-dir "~/.dsh/group"
  "Path to user dsh group configuration files."
  :group 'ssh-config
  :type 'string)

(defcustom sc:ssh-config-keywords
  '("Host" "AddressFamily" "BatchMode" "BindAddress"
    "ChallengeResponseAuthentication" "CheckHostIP" "Cipher" "Ciphers"
    "ClearAllForwardings" "Compression" "CompressionLevel" "ConnectionAttempts"
    "ConnectTimeout" "ControlMaster" "ControlPath" "ControlPersist" "DynamicForward"
    "EnableSSHKeysign" "EscapeChar" "ExitOnForwardFailure" "ForwardAgent"
    "ForwardX11" "ForwardX11Timeout" "ForwardX11Trusted" "GatewayPorts" "GlobalKnownHostsFile"
    "GSSAPIAuthentication" "GSSAPIKeyExchange" "GSSAPIClientIdentity"
    "GSSAPIDelegateCredentials" "GSSAPIRenewalForcesRekey" "GSSAPITrustDns"
    "HashKnownHosts" "HostbasedAuthentication" "HostKeyAlgorithms"
    "HostKeyAlias" "HostName" "IdentitiesOnly" "IdentityFile" "IPQoS"
    "KbdInteractiveAuthentication" "KbdInteractiveDevices" "KexAlgorithms" "LocalCommand"
    "LocalForward" "LogLevel" "MACs" "NoHostAuthenticationForLocalhost"
    "NumberOfPasswordPrompts" "PasswordAuthentication" "PermitLocalCommand"
    "PKCS11Provider" "Port" "PreferredAuthentications" "Protocol"
    "ProxyCommand" "PubkeyAuthentication" "RekeyLimit" "RemoteForward" "RequestTTY"
    "RhostsRSAAuthentication" "RSAAuthentication" "SendEnv"
    "ServerAliveCountMax" "ServerAliveInterval" "StrictHostKeyChecking"
    "TCPKeepAlive" "Tunnel" "TunnelDevice" "UseBlacklistedKeys"
    "UsePrivilegedPort" "User" "UserKnownHostsFile" "VerifyHostKeyDNS"
    "VisualHostKey" "XAuthLocation")
  "List of keywords for ssh client configuration files as defined
in `ssh_config(5)'."
  :group 'ssh-config
  :type 'list)

(defcustom sc:ssh-proxy-command 
  '("ssh" "-o" "ConnectTimeout=5" "-q" "-t" jumphost "nc" "-w" "60" targethost "%p"))
  "Default proxy command for ssh configuration. This string is passed to
`format' with proxy host as argument."
  :group 'ssh-config
  :type 'string)

(defcustom sc:ssh-file "~/.ssh/hosts.org"
  "Path to hosts configuration."
  :group 'ssh-config
  :type 'string)

(defcustom sc:extra-args `("-l" ,(concat (file-name-as-directory
					  user-emacs-directory)
					 "init.el")
			   "--eval" "(require (quote ssh-config))")
  "Extra arguments added to `command-line-args' during
  `gen-ssh-config-async'.

  For debugging purposes, you can add \"--eval\" \"\(setq
  debug-on-error t\)\" and \"--debug-init\".")


(defstruct ssh-host
  name
  tags
  ssh-opts
  proxy
  todo)



(defun ssh-gen-expand-command(command vars)
  "Expand COMMAND list using VARS PLIST.

For each element E of COMMAND:

 - If E is a symbol and defined in VARS, its value is taken from
   from VARS if not nil.
 - If E is a LIST, it is `eval'.
 - Else E is taken as it.
"
  (loop for c in command
	if (symbolp c)
	collect (or (plist-get vars c) c)
	else
	if (listp c)
	collect (eval c)
	else
	collect c))



(defun ssh-gen-parse-hosts(&optional file)
  "Parse ssh configuration from `sc:ssh-file' org file or from FILE
if defined."
  (let* ((file (or file sc:ssh-file)))

    (with-current-buffer (or
			  (get-file-buffer file)
			  (find-file file))

      (let ((markers (org-map-entries 'point-marker
				      "/!-SKIP-DISABLED"
				      'file)))

	(loop for marker in markers
	      collect (org-with-point-at marker
			(let* ((props (org-entry-properties marker 'all))
			       (todo (cdr (assoc "TODO" props)))
			       (parents (unless (string= "DIRECT" todo)
					  (list (car (last (org-get-outline-path))))))
			       ssh-opts proxy)

			  (loop for prop in props
				when (member (car prop) sc:ssh-config-keywords)
				do (add-to-list 'ssh-opts (format "%s %s" (car prop) (cdr prop)))
				when (string= (car prop) "Other-Path")
				do (setf parents (append parents (split-string (cdr prop)))))


			  (setq proxy
				(loop for prxy in
				      (loop for parent in parents
					    collect
					    (let ((path (split-string parent ",")))
					      (ssh-gen-expand-command
					       sc:ssh-proxy-command
					       `(jumphost ,(car path)
							  targethost ,(or (cadr path) "%h")))))
				      collect (mapconcat 'identity prxy " ")))


			  (looking-at org-complex-heading-regexp)

			  (make-ssh-host
			   :name (org-match-string-no-properties 4)
			   :tags (org-get-local-tags)
			   :ssh-opts ssh-opts
			   :proxy (when proxy
				    (concat "ProxyCommand "
					    (if (> (length proxy) 1)
						(concat "sh -c \""
							(mapconcat 'identity proxy " || ")
							"\"")
					      (car proxy))))
			   :todo todo))))))))

;;;###autoload
(defun ssh-gen-config()
  "Generate ssh configuration from `sc:ssh-file' org file."
  (interactive)
  (let* ((hosts (ssh-gen-parse-hosts))
	 (tags (loop for host in hosts
		     with ret = (make-hash-table :test 'equal)
		     do (loop for tag in (ssh-host-tags host)
			      do (let ((h (gethash tag ret)))
				   (add-to-list 'h (ssh-host-name host))
				   (puthash tag h ret)))
		     finally return ret)))
    tags))


;;;###autoload
(defun ssh-gen-config()
  "Generate ssh configuration from `sc:ssh-file' org file."
  (interactive)
  (save-selected-window
    (let (;; Disable slowing checks
	  (backup-inhibited t)
	  (auto-save-default nil)
	  (find-file-hook nil)
	  (after-save-hook nil)
	  (org-mode-hook nil)
	  (before-save-hook nil)
	  (vc-follow-symlinks t)
	  (bconfig "*ssh config*")
	  markers buffers kill-bufferp)
      (unless (find-buffer-visiting sc:ssh-file)
	(setq kill-bufferp t))
      (find-file sc:ssh-file)
      (unless (eq 'org-mode major-mode)
	(error "File %s is no in `org-mode'" sc:ssh-file))
      ;; Scan the file for host definition
      (org-scan-tags
       '(add-to-list 'markers (set-marker (make-marker) (point)))
       '(not (member
	      (cdr (assoc "TODO" (org-entry-properties (point) 'all)))
	      '("SKIP" "DISABLED"))))
      (get-buffer-create bconfig)
      (loop for marker in (nreverse markers)
	    do (org-with-point-at marker
		 (looking-at org-complex-heading-regexp)
		 (let* ((host	(org-match-string-no-properties 4))
			(props  (org-entry-properties marker 'all))
			(tags   (org-get-tags))
			(parent (car (last (org-get-outline-path))))
			(todo   (assoc "TODO" props))
			other-path)
		   (set-buffer bconfig)
		   (insert (format "Host %s\n" host))
		   ;; Insert all ssh keywords
		   (loop for prop in props
			 do (cond
			     ((member (car prop) sc:ssh-config-keywords)
			      (insert (format "\t%s %s\n"
					      (car prop) (cdr prop))))
			     ((string= (car prop) "Other-Path")
			      (setq other-path (split-string (cdr prop))))))
		   ;; If no TODO is found and host has a parent, use
		   ;; parent as a proxy
		   (cond 
		    ((and parent (not todo))
		     ;; Multipath:
		     (if other-path
			 (insert (format "\tProxyCommand sh -c \"%s\""
					 (mapconcat
					  'identity
					  (loop for h in (cons parent other-path)
						collect
						(let* ((split-h (split-string h ","))
						       (proxy (car split-h))
						       (host-target (or (cadr split-h) "%h")))
						  
						  (format sc:ssh-proxy-command proxy host-target)))
					  " || ")))
		       (insert (format "\tProxyCommand %s"
				       (format sc:ssh-proxy-command parent "%h")))))
		    ((and other-path todo)
		     (when other-path
		       (insert (format "\tProxyCommand sh -c \"%s\""
				       (mapconcat
					'identity
					(loop for h in other-path
					      collect
					      (let* ((split-h (split-string h ","))
						     (proxy (car split-h))
						     (host-target (or (cadr split-h) "%h")))
						
						  (format sc:ssh-proxy-command proxy host-target)))
					" || "))))))
		   (insert "\n")
		   ;; retrieve all groups
		   (loop for tag in tags
			 do (progn
			      (set-buffer
			       (get-buffer-create (format "*dsh group %s*" tag)))
			      (insert (format "%s\n" host))
			      (add-to-list 'buffers (current-buffer)))))))
      (set-buffer bconfig)
      (write-file sc:ssh-config-file)
      (kill-buffer (current-buffer))
      (unless (file-exists-p sc:dsh-config-dir)
	(mkdir sc:dsh-config-dir))
      (save-match-data
      	(loop for buffer in buffers
      	      do (progn
      		   (set-buffer buffer)
      		   (when (string-match "\\*dsh group \\(.*\\)\\*" (buffer-name))
      		     (write-file
      		      ;; change "_" in "-" for group name.
      		      (concat (file-name-as-directory sc:dsh-config-dir)
      			      (replace-regexp-in-string
      			       "_" "-" (match-string 1 (buffer-name)))))
      		     (kill-buffer (current-buffer))))))
      (when kill-bufferp
	(kill-buffer (find-buffer-visiting sc:ssh-file))))))


(defun ssh-config-async-sentinel (proc change)
  "Sentinel in charge of running next process if previous one succeeded."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd (process-get proc :cmd))
	  (cmd-buf (process-get proc :cmd-buf)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) cmd-buf))
	    (error "SSH config ERROR: %s" cmd))
	(message  "SSH config OK: %s" cmd))
      (when cmd-buf (kill-buffer cmd-buf)))))

;;;###autoload
(defun ssh-gen-config-async ()
  "Asynchronous files generation."
  (interactive)
  (let* ((cmd-line (append command-line-args
			   `("--batch" ,@sc:extra-args
			     "--eval" "(ssh-gen-config)")))
	 (cmd-buf (get-buffer-create "SSH configuration async gen"))
	 (proc (apply 'start-process (car cmd-line)
		      cmd-buf (car cmd-line) (cdr cmd-line))))
    (message (format "%S" cmd-line))
    (process-put proc :cmd sc:ssh-file)
    (process-put proc :cmd-buf cmd-buf)
    (set-process-sentinel proc 'ssh-config-async-sentinel)))

(provide 'ssh-config)
