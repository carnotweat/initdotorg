;; bitlbee & telegram
(defun i-wanna-be-social ()
  "Connect to IM networks using bitlbee."
  (interactive)
  (erc :server "localhost" :port 6667 :nick "meraesg"))
(defun erc-cmd-SHOWOFF (&rest ignore)
    "Show off implementation"
    (let* ((chnl (erc-buffer-list))
           (srvl (erc-buffer-list 'erc-server-buffer-p))
           (memb (apply '+ (mapcar (lambda (chn)
                                     (with-current-buffer chn
                                       (1- (length (erc-get-channel-user-list)))))
                                   chnl)))
           (show (format "is connected to %i networks and talks in %i chans to %i ppl overall :>"
                         (length srvl)
                         (- (length chnl) (length srvl))
                         memb)))
      (erc-send-action (erc-default-target) show)))
  (defalias 'erc-cmd-SO 'erc-cmd-SHOWOFF)
  
  (defun erc-cmd-DETAILED-SHOWOFF (&rest ignore)
    "Show off implementation enriched with even more with details"
    (let* ((chnl (erc-buffer-list))
           (srvl (erc-buffer-list 'erc-server-buffer-p)))
      (mapcar (lambda (srv)
                (let* ((netn (with-current-buffer srv erc-session-server))
                       (netp (with-current-buffer srv erc-session-port))
                       (chns (remove-if-not
                              (lambda (chn)
                                (and (string= netn (with-current-buffer chn erc-session-server))
                                     (eq netp (with-current-buffer chn erc-session-port))))
                              chnl))
                       (chnn (1- (length chns)))
                       (chnm (remove nil
                                     (mapcar (lambda (chn)
                                               (with-current-buffer chn
                                                 (erc-get-channel-user-list)))
                                             chns)))
                       (chnmn (apply '+ (mapcar '1- (mapcar 'length chnm))))
                       (show (format "is connected to %s (%s), talking to %i users in %i chans"
                                     netn
                                     (buffer-name srv)
                                     chnmn
                                     chnn)))
                  (erc-send-action (erc-default-target) show)
                  (sit-for 1)))
              srvl)))
  (defalias 'erc-cmd-DSO 'erc-cmd-DETAILED-SHOWOFF)
