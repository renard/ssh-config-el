;;; ssh-config.el --- Manage both ssh and dsh confguration from emacs

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, ssh
;; Created: 2010-11-22
;; Last changed: 2012-05-02 11:02:03
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
  '("ssh" "-o" "ConnectTimeout=5" "-q" "-t" jumphost "nc" "-w" "60" targethost "%p")
  "Default proxy command for ssh configuration. This string is passed to
`ssh-gen-expand-command' with variables:

  - jumphost: a proxy host
  - targethost: the destination host."
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
				      "/-SKIP-DISABLED"
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
			   :tags (loop for tag in (org-get-local-tags)
				       collect (replace-regexp-in-string
						"_" "-" tag))
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

    ;; ssh config
    (with-temp-buffer
      (loop for host in hosts
	    do (progn
		 (insert (format "Host %s\n" (ssh-host-name host)))

		 (loop for o in (ssh-host-ssh-opts host)
		       do (insert (format "\t%s\n" o)))

		 (when (ssh-host-proxy host)
		   (insert
		    (format "\t%s\n" (ssh-host-proxy host))))

		 (insert "\n")))
      (write-file sc:ssh-config-file))

    ;; DSH
    (make-directory sc:dsh-config-dir t)
    (loop for tag being the hash-key in tags
	  do (with-temp-buffer
	       (insert (mapconcat 'identity (gethash tag tags) "\n"))
	       (write-file (format "%s/%s" sc:dsh-config-dir tag))))))


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
