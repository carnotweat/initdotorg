;;; colorg.el --- Emacs Lisp part of real-time collaboration for Org mode.

;; Copyright © 2013 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; URL: https://github.com/pinard/colorg

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; colorg is a real-time collaborative editing tool originally meant
;; for Emacs Org mode, but usable more generally.  It requires a
;; running colorg server.  See https://github.com/pinard/colorg/wiki/.

;;; Code:

;; The code is organized into pages, grouping declarations by topic.
;; Such pages are introduced by a form feed and a topic description.
;; Variables START and END refer to Emacs positions, which are one-based.
;; If ever needed, use FIRST for colorg positions, which are zero-based.

;;; User parameterization.

(defconst colorg-welcome "colorg v1"
  "Welcome string expected when initially contacting a colorg server.")

(defvar colorg-user-nickname (user-login-name)
  "Name by which this user is identified to other collaborators.")

(defvar colorg-accept-timeout 5
  "Number of seconds to wait after the colorg server.")

(defvar colorg-idle-timeout 1
  "Number of quiescent second before polling the colorg server.")

(defvar colorg-hue-bias (progn (random t) (* 0.00001 (random 100000)))
  "Bias for hue, so colors are never predictable between Emacs sessions.
You may change that to a floating constant between 0.0 and 1.0 (excluded)
if you always want the same colors for the same user numbers.")

(defvar colorg-notification-timeout 4
  "Number of seconds to keep each notification displayed.
Instead of a number, the value 'ask waits for the user to type Enter.
The special value 'org rather requests Org notifications.")

(defvar colorg-notification-beep t
  "Use nil to inhibit notification sounds.")

;;; Activation and deactivation.

(defvar colorg-buffer-alist nil
  "List of (BUFFER RESOURCE SERVER) triplets.
A monitored BUFFER is associated to a RESOURCE number and a SERVER connection.
Both are needed: as each server uses it own numbering, numbers may clash.  This
COLORG-BUFFER-ALIST structure replaces buffer-local variables which would be
more attractive, if only major modes did not tamper with them unexpectedly.")

(define-minor-mode colorg-mode
  "Collaborative editing mode.
See https://github.com/pinard/colorg/wiki/ for more information."
  nil " co" nil
  (if colorg-mode
      ;; Turning mode on.
      (let ((buffer (current-buffer))
            server resource)
        (condition-case err
            (setq server (colorg-select-server)
                  resource (colorg-associate-resource server))
          (error (setq colorg-mode nil)
                 (error "Error activating colorg: %s"
                        (error-message-string err))))
        (push (list (current-buffer) resource server) colorg-buffer-alist)
        (save-excursion
          (set-buffer server)
          (push (cons resource buffer) co-alist)))
    ;; Turning mode off.
    (let* ((buffer (current-buffer))
           (data (assq buffer colorg-buffer-alist))
           (resource (nth 1 data))
           (server (nth 2 data)))
      (when data
        (colorg-round-trip (list 'leave resource) server)
        (save-excursion
          (set-buffer server)
          (setq co-alist (delq (assq resource co-alist) co-alist)))
        (setq colorg-buffer-alist (delq data colorg-buffer-alist))))))

(defun colorg-global-enable ()
  (interactive)
  (add-hook 'after-change-functions 'colorg-after-change-routine)
  (setq colorg-idle-timer
        ;; FIXME: run-with-idle-timer is the real goal, but I do not
        ;; succeed in firing it frequently enough.
        (run-with-timer 0.1 colorg-idle-timeout 'colorg-idle-routine)))

(defun colorg-global-disable ()
  (interactive)
  (remove-hook 'after-change-functions 'colorg-after-change-routine)
  (cancel-timer colorg-idle-timer))

(colorg-global-enable)

(defun colorg-kill-buffer-routine ()
  "Unregister from the server when killing a buffer in colorg mode."
  (when colorg-mode
    (colorg-mode 0)))

(add-hook 'kill-buffer-hook 'colorg-kill-buffer-routine)

;;; Command processing.

(defun colorg-associate-resource (server)
  "Select one of SERVER's resource interactively and return its number.
If the resource already exists, associate the current buffer with
the resource if the contents match.  If they do not match and the
buffer is empty, download the resource in the buffer.  Otherwise,
create a new resource and upload its contents from the buffer."
  (let* ((pairs (colorg-round-trip 'resources server))
         (name (and pairs
                    (completing-read "Existing resource? " pairs nil t)))
         resource string start)
    (if (and name (not (string-equal name "")))
        (let ((md5sum
               (let ((output (generate-new-buffer " *md5sum*")))
                 (unwind-protect
                     (progn
                       (call-process-region
                        (point-min) (point-max) "md5sum" nil output)
                       (save-excursion
                         (set-buffer output)
                         (goto-char (point-max))
                         (skip-chars-backward "- \n")
                         (buffer-substring-no-properties (point-min) (point))))
                   (kill-buffer output)))))
          (setq resource
                (car (colorg-round-trip (list 'join name md5sum) server))))
      (setq name (read-string "New resource name? " nil nil (buffer-name)))
      (when (assoc name pairs)
        (error "This resource already exists."))
      (setq resource (car (colorg-round-trip (list 'create name) server))
            insert (buffer-substring-no-properties (point-min) (point-max))
            start (point-min))
      (save-excursion
        (set-buffer server)
        (push (list 'alter resource (1- start) 0 insert) co-outgoing)))
    resource))

(defun colorg-select-resource (server)
  "Select a SERVER resource interactively, return its number, else nil."
  (let* ((pairs (colorg-round-trip 'resources server))
         (name (completing-read "Which resource? " pairs nil t)))
    (and name (cadr (assoc name pairs)))))

(defun colorg-select-user (server)
  "Select a SERVER user interactively, return its number, else nil."
  (let* ((pairs (colorg-round-trip 'users server))
         (name (completing-read "Which user? " pairs nil t)))
    (and name (cadr (assoc name pairs)))))

(defun colorg-send-message (text)
  "Send some chat TEXT to some interactively selected user.
The current buffer's colorg server is queried for possible users."
  (interactive "sMessage? ")
  (let* ((buffer (current-buffer))
         (data (assq buffer colorg-buffer-alist))
         (server (nth 2 data)))
    (unless server
      (error "No server found for this buffer"))
    (colorg-round-trip (list 'chat (colorg-select-user server) text)
                       server)))

;;; Hooks monitoring user actions.

(defun colorg-after-change-routine (start end deleted)
  "After any buffer change, tell the server about the alter to do.
These commands are accumulated and sent at regular intervals."
  (when colorg-mode
    (let* ((insert (buffer-substring-no-properties start end))
           (data (assq (current-buffer) colorg-buffer-alist))
           (resource (nth 1 data))
           (server (nth 2 data)))
      (save-excursion
        (set-buffer server)
        (push (list 'alter resource (1- start) deleted insert)
              co-outgoing)))))

(defun colorg-idle-routine ()
  "Whenever Emacs gets idle, round trip with the synchronization server.
We push out accumulated commands.  Then, we get externally
triggered alter commands from the server and execute them all."
  (save-match-data
    (let ((pairs colorg-server-buffers))
      (while pairs
        (let ((server (cdr (pop pairs)))
              outgoing)
          (save-excursion
            (set-buffer server)
            (setq outgoing co-outgoing)
            (setq co-outgoing nil))
          ;; Once incoming dispatch works, move this in the save-excursion.
          (colorg-round-trip (if outgoing
                                 (cons 'poll (nreverse outgoing))
                               'poll)
                             server))))))
;;(timer-set-idle-time colorg-idle-timer colorg-idle-timeout)

;;; Servers and communication.

(defvar colorg-server-buffers nil
  "Association list relating server names to server buffers.")

(defvar colorg-idle-timer nil
  "Timer used to detect the quiescence of Emacs.")

(require 'json)

(defun colorg-select-server ()
  "Select a server interactively, and return its buffer.
If the server does not exist yet, create it."
  (let* ((name (completing-read "Which server? " colorg-server-buffers))
         (pair (assoc name colorg-server-buffers)))
    (unless pair
      (when (string-equal name "")
        (cond ((not colorg-server-buffers)
               (setq name "local"))
              ((= (length colorg-server-buffers) 1)
               (setq pair (car colorg-server-buffers)
                     name (car pair)))
              (t (error "Many servers, please select one."))))
      (unless pair
        (let ((host (read-string (format "Server %s host? " name)
                                 "localhost"))
              (port (string-to-number
                     (read-string (format "Server %s port? " name)
                                  "7997")))
              (server (get-buffer-create (format " *colorg %s*" name))))
          (save-excursion
            (set-buffer server)

            ;; Server nickname, for diagnostic purposes.
            (set (make-local-variable 'co-name) name)

            ;; Network stream reaching the colorg server.
            (set (make-local-variable 'co-process)
                 (open-network-stream name server host port))
            (set-process-query-on-exit-flag co-process nil)

            ;; Make sure the server replies correctly.
            (erase-buffer)
            (while (not (search-forward "\n" nil t))
              (goto-char (point-max))
              (let ((here (point)))
                (accept-process-output co-process colorg-accept-timeout)
                (when (= here (point-max))
                  (colorg-mode 0)
                  (error "colorg disabled: server does not seem to reply.")))
              (goto-char (point-min)))
            (goto-char (point-min))
            (let* ((json-array-type 'list)
                   (reply (json-read)))
              (unless (string-equal reply colorg-welcome)
                (error "colorg disabled: server sent %S instead of %S"
                       reply colorg-welcome)))

            ;; Association list relating a resource number to a local
            ;; buffer in colorg mode.
            (set (make-local-variable 'co-alist) nil)

            ;; Alter commands meant to be broadcasted as a reversed
            ;; list: the most recently accumulated command first.
            ;; These are sent to the server at regular time intervals.
            (set (make-local-variable 'co-outgoing) nil)

            ;; Our own user number on this server.
            (set (make-local-variable 'co-user)
                 (car (colorg-round-trip (list 'login colorg-user-nickname)
                                         server))))

          ;; Register this server buffer.
          (setq pair (cons name server))
          (push pair colorg-server-buffers))))
    (cdr pair)))

(defun colorg-round-trip (command server)
  "Send COMMAND to SERVER, receive and process reply, then return values."
  (save-excursion
    (set-buffer server)
    (unless (and (processp co-process)
                 ;; FIXME: Because I restart the server?  Not healthy!
                 (eq (process-status co-process) 'open))
      (error "Server %s process vanished." co-name))
    (erase-buffer)
    (process-send-string nil (concat (json-encode command) "\n"))
    (while (not (search-forward "\n" nil t))
      (goto-char (point-max))
      (let ((here (point)))
        (accept-process-output co-process colorg-accept-timeout)
        (when (= here (point-max))
          (colorg-mode 0)
          (error "colorg disabled: server does not seem to reply.")))
      (goto-char (point-min)))
    (goto-char (point-min))
    (colorg-reply-handler (let ((json-array-type 'list))
                            (json-read)))))

(defun colorg-reply-handler (command)
  "Process COMMAND, then return values.  The server buffer should be current."
  (let (action arguments values)
    (if (stringp command)
        (setq action (intern command)
              arguments nil)
      (setq action (intern (car command))
            arguments (cdr command)))
    (cond ((eq action 'alter)
           (let ((inhibit-modification-hooks t)
                 (inhibit-point-motion-hooks t)
                 (resource (nth 0 arguments))
                 (start (1+ (nth 1 arguments)))
                 (deleted (nth 2 arguments))
                 (insert (nth 3 arguments))
                 (user (nth 4 arguments)))
             (let ((buffer (cdr (assq resource co-alist))))
               (if buffer
                   (save-excursion
                     (set-buffer buffer)
                     (delete-region start (+ start deleted))
                     (goto-char start)
                     (when (numberp insert)
                       (setq insert (make-string insert '?')))
                     (insert-before-markers insert)
                     (colorg-colorize start (+ start (length insert)) user))
                 (message "colorg-reply-handler: No local buffer to handle %S"
                          command)))))
          ((eq action 'chat)
           (let ((user (nth 0 arguments))
                 (string (nth 1 arguments)))
             (colorg-notify
              (concat
               (propertize (format "User %s:" user)
                           'face (colorg-face-name user) 'weight "bold")
               " " string))))
          ((eq action 'done)
           (setq values arguments))
          ((eq action 'exec)
           (setq values (append (mapcar 'colorg-reply-handler arguments))))
          ((eq action 'error)
           (let ((string (nth 0 arguments)))
             (colorg-mode 0)
             (colorg-notify
              (concat
               (propertize "Error (disabling): " 'face 'font-lock-warning-face)
               string))))
          ((eq action 'warn)
           (let ((string (nth 0 arguments)))
             (colorg-notify
              (concat
               (propertize "Warning: " 'face 'font-lock-warning-face)
               string))))
          (t (debug)))
    values))

;;; Coloration matters.

(defconst colorg-phi (* 0.5 (1+ (sqrt 5)))
  "Golden ratio.")

(defvar colorg-overlays nil
  "Association between keys and overlays.")

(defun colorg-colorize (start end key)
  "Highlight current buffer region from START to END with a color tied to KEY.
If the region is empty, extend it so it contains one character.
The first time a key appears, automatically select a color for it.
Else, first remove the previous highlight made for that key."
  (unless (< start end)
    (cond ((> start (point-min)) (setq end start
                                       start (1- start)))
          ((< start (point-max)) (setq end (1+ start)))))
  (let ((pair (assoc key colorg-overlays)))
    (if pair
        (move-overlay (cdr pair) start end (current-buffer))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face (colorg-face-name key))
        (push (cons key overlay) colorg-overlays)))))

(defun colorg-face-name (key)
  "Return the face name, as a symbol, associated with user KEY.
Create a new face if it does not exist already."
  (let ((symbol (intern (format "colorg-face-%d" key))))
    (unless (facep symbol)
      (let ((rgb (colorg-hsv-to-rgb
                  (+ colorg-hue-bias (* colorg-phi key)) 0.25 1.0)))
        (set-face-background (make-face symbol)
                             (format "#%02x%02x%02x"
                                     (floor (* 255.0 (nth 0 rgb)))
                                     (floor (* 255.0 (nth 1 rgb)))
                                     (floor (* 255.0 (nth 2 rgb)))))))
    symbol))

(defun colorg-hsv-to-rgb (hue saturation value)
  "Convert an HSV color to RGB.
Only the fractional part of HUE is used, so it gets within [0.0, 1.0).
SATURATION and VALUE are both to be given within [0.0, 1.0].
Returns a list (RED GREEN BLUE), each within [0.0, 1.0].
Adapted from Adrian Aichner code, see http://emacswiki.org/emacs/hsv2rgb.el."
  (setq saturation (float saturation)
        value (float value))
  (let* ((6*hue (* hue 6.0))
         (index (mod (floor 6*hue) 6))
         (fraction (mod 6*hue 1.0))
         (x (* value (- 1.0 saturation)))
         (y (* value (- 1.0 (* fraction saturation))))
         (z (* value (- 1.0 (* (- 1.0 fraction) saturation)))))
    (cond ((= index 0) (list value z x))
          ((= index 1) (list y value x))
          ((= index 2) (list x value z))
          ((= index 3) (list x y value))
          ((= index 4) (list z x value))
          (t (list value x y)))))

;;; Notifications.

(defvar colorg-notification-buffer-name "*colorg-notification*"
  "Name of colorg notification buffer.")

(defvar colorg-notification-is-displayed nil
  "A notification is currently displayed.")

(defvar colorg-pending-notifications nil
  "Notifications waiting to be displayed.")

(defun colorg-notify (text)
  "Manage to notify the user with TEXT as a message."
  (message "colorg-notify: %s" text)
  (message "")
  (cond ((eq colorg-notification-timeout 'org)
         (org-notify text colorg-notification-beep))
        ((eq colorg-notification-timeout 'ask)
         (colorg-show-notification text)
         (read-string "Got it? [Enter] ")
         (colorg-hide-notification))
        (t (add-to-list 'colorg-pending-notifications text t)
           (unless colorg-notification-is-displayed
             (colorg-advance-notifications)))))

(defun colorg-advance-notifications ()
  (when colorg-notification-is-displayed
    (colorg-hide-notification))
  (when colorg-pending-notifications
    (colorg-show-notification (pop colorg-pending-notifications))
    (run-at-time colorg-notification-timeout nil
                 'colorg-advance-notifications)))

(defun colorg-show-notification (text)
  "Display TEXT as a notification, in a separate buffer."
  ;; Adapted from appt.el.
  (when colorg-notification-beep
    (beep))
  (let ((this-window (selected-window))
        (buffer (get-buffer-create colorg-notification-buffer-name)))
    ;; Make sure we're not in the minibuffer before splitting the window.
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
	(progn
	  (set-buffer buffer)
	  (display-buffer buffer))
      (unless (or (special-display-p (buffer-name buffer))
                  (same-window-p (buffer-name buffer)))
        ;; By default, split the bottom window and use the lower part.
        (appt-select-lowest-window)
        ;; Split the window, unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer buffer))
    (setq buffer-read-only nil
          buffer-undo-list t
          mode-line-format nil)
    (erase-buffer)
    (insert text)
    (let ((window-min-height 1))
       (shrink-window-if-larger-than-buffer (get-buffer-window buffer t)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame (selected-frame))
    (select-window this-window))
  (setq colorg-notification-is-displayed t))

(defun colorg-hide-notification ()
  "Function called to undisplay the notification message."
  ;; Adapted from appt.el.
  (setq colorg-notification-is-displayed nil)
  (let ((window (get-buffer-window colorg-notification-buffer-name t)))
    (and window
         (or (eq window (frame-root-window (window-frame window)))
             (delete-window window))))
  (kill-buffer colorg-notification-buffer-name))

(defun colorg-select-lowest-window ()
  "Select the lowest window on the frame."
  ;; Stolen from appt.el.
  (let ((lowest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (window)
                    (when (< bottom-edge
                             (setq next-bottom-edge
                                   (nth 3 (window-edges window))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window window)))
                  'except-minibuffer)
    (select-window lowest-window)))

(provide 'colorg)
;;; colorg.el ends here
