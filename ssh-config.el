;;; ssh-config.el --- Manage both ssh and dsh confguration from emacs

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, ssh
;; Created: 2010-11-22
;; Last changed: 2010-12-14 17:02:14
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; 

;;; Code:

(require 'org)

(defcustom sc:ssh-config-file "~/.ssh/config"
  "Path to user ssh configuration file."
  :group 'ssh-config
  :type 'string)

(defcustom sc:dsh-config-file "~/.dsh/group/%s"
  "Path to user dsh group configuration files."
  :group 'ssh-config
  :type 'string)

(defcustom sc:ssh-config-keywords
  '("Host" "AddressFamily" "BatchMode" "BindAddress"
    "ChallengeResponseAuthentication" "CheckHostIP" "Cipher" "Ciphers"
    "ClearAllForwardings" "Compression" "CompressionLevel" "ConnectionAttempts"
    "ConnectTimeout" "ControlMaster" "ControlPath" "DynamicForward"
    "EnableSSHKeysign" "EscapeChar" "ExitOnForwardFailure" "ForwardAgent"
    "ForwardX11" "ForwardX11Trusted" "GatewayPorts" "GlobalKnownHostsFile"
    "GSSAPIAuthentication" "GSSAPIKeyExchange" "GSSAPIClientIdentity"
    "GSSAPIDelegateCredentials" "GSSAPIRenewalForcesRekey" "GSSAPITrustDns"
    "HashKnownHosts" "HostbasedAuthentication" "HostKeyAlgorithms"
    "HostKeyAlias" "HostName" "IdentitiesOnly" "IdentityFile"
    "KbdInteractiveAuthentication" "KbdInteractiveDevices" "LocalCommand"
    "LocalForward" "LogLevel" "MACs" "NoHostAuthenticationForLocalhost"
    "NumberOfPasswordPrompts" "PasswordAuthentication" "PermitLocalCommand"
    "PKCS11Provider" "Port " "PreferredAuthentications" "Protocol"
    "ProxyCommand" "PubkeyAuthentication" "RekeyLimit" "RemoteForward"
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
  "\tProxyCommand ssh -q -t %s nc -w 1 %%h %%p"
  "Default proxy command for ssh configuration. This string is passed to
`format' with proxy host as argument."
  :group 'ssh-config
  :type 'string)

(defcustom sc:ssh-file "~/.ssh/hosts.org"
  "Path to hosts configuration."
  :group 'ssh-config
  :type 'string)


(defun ssh-gen-config()
  "Generate ssh configuration from `sc:ssh-file' org file."
  (interactive)
  (save-selected-window
    (let ((bconfig "*ssh config*")
	  markers buffers kill-bufferp)
      (unless (find-buffer-visiting sc:ssh-file)
	(setq kill-bufferp t))
      (find-file sc:ssh-file)
      (unless (org-mode-p)
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
			(todo   (assoc "TODO" props)))
		   (set-buffer bconfig)
		   (insert (format "Host %s\n" host))
		   ;; Insert all ssh keywords
		   (loop for prop in props
			 do (when (member (car prop) test:ssh-config-properties)
			      (insert (format "\t%s %s\n"
					      (car prop) (cdr prop)))))
		   ;; If no TODO is found and host has a parent, use
		   ;; parent as a proxy
		   (when (and parent (not todo))
		     (insert (format test:ssh-config-ssh-proxy-command parent)))
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
      (save-match-data
	(loop for buffer in buffers
	      do (progn
		   (set-buffer buffer)
		   (when (string-match "\\*dsh group \\(.*\\)\\*" (buffer-name))
		     (write-file
		      ;; change "_" in "-" for group name.
		      (format sc:dsh-config-file
			      (replace-regexp-in-string 
			       "_" "-" (match-string 1 (buffer-name)))))
		     (kill-buffer (current-buffer))))))
      (when kill-bufferp
	(kill-buffer (find-buffer-visiting sc:ssh-file))))))

(provide 'ssh-config)
