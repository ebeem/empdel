;;; empdel.el --- An ncmpcpp clone for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 ebeem

;; Author: Ebeem <ibeem@outlook.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, mpd
;; URL: https://github.com/ebeem/empdel

;;; Commentary:
;; 
;; A feature-rich MPD (Music Player Daemon) client inspired by ncmpcpp.
;; 
;; Provides 5 core views for managing your music:
;; 1. Current Playlist  (Press '1')
;; 2. File Browser      (Press '2')
;; 3. Search Engine     (Press '3')
;; 4. Media Library     (Press '4')
;; 5. Playlist Editor   (Press '5')

;;; Code:

(require 'tabulated-list)

(defgroup empdel nil
  "A feature-rich MPD client inspired by ncmpcpp."
  :group 'multimedia
  :prefix "empdel-")

(defcustom empdel-mpd-host "localhost"
  "Host where MPD is running."
  :type 'string)

(defcustom empdel-mpd-port 6600
  "Port where MPD is listening."
  :type 'integer)

(defface empdel-playing-song-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face used to highlight the currently playing song in the playlist."
  :group 'empdel)

;;; --- Network Communication ---

(defun empdel--send-command (command)
  "Send COMMAND to MPD and return the response as a string."
  (condition-case err
      (with-temp-buffer
        (let ((proc (make-network-process :name "empdel-proc"
                                          :buffer (current-buffer)
                                          :host empdel-mpd-host
                                          :service empdel-mpd-port)))
          ;; Wait for MPD's initial greeting
          (while (= (buffer-size) 0)
            (accept-process-output proc 0.1))
          (erase-buffer)
          
          ;; Send the command
          (process-send-string proc (concat command "\n"))
          
          ;; Wait for "OK" or "ACK" to finish
          (goto-char (point-min))
          (while (not (re-search-forward "^\\(OK\\|ACK\\)" nil t))
            (accept-process-output proc 0.1)
            (goto-char (point-min)))
          
          ;; Close connection
          (process-send-string proc "close\n")
          (delete-process proc)
          (buffer-string)))
    (error (error "Empdel: Cannot connect to MPD at %s:%d. Is it running? (%s)" 
                  empdel-mpd-host empdel-mpd-port (error-message-string err)))))

(defun empdel--parse-playlist (response)
  "Parse MPD playlist RESPONSE into a list of alists."
  (let ((lines (split-string response "\n" t))
        (current-song nil)
        (playlist nil))
    (dolist (line lines)
      (when (string-match "^\\(.*?\\): \\(.*\\)$" line)
        (let ((key (match-string 1 line))
              (val (match-string 2 line)))
          (if (string= key "file")
              (progn
                (when current-song
                  (push (nreverse current-song) playlist))
                (setq current-song (list (cons 'file val))))
            (push (cons (intern (downcase key)) val) current-song)))))
    (when current-song
      (push (nreverse current-song) playlist))
    (nreverse playlist)))

(defun empdel--format-time (seconds-str)
  "Format SECONDS-STR into MM:SS."
  (if (not seconds-str)
      "00:00"
    (let ((secs (string-to-number seconds-str)))
      (format "%02d:%02d" (/ secs 60) (% secs 60)))))

(defun empdel--safe-quote (str)
  "Safely escape quotes and backslashes in STR for MPD."
  ;; prin1-to-string adds surrounding quotes and correctly escapes \ and "
  ;; substring-no-properties ensures invisible Emacs formatting is stripped out
  (prin1-to-string (substring-no-properties str)))

;;; --- Core MPD Commands ---

(defun empdel-toggle-play ()
  "Toggle play/pause status."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (state (if (string-match "^state: \\(.*\\)$" status)
                    (match-string 1 status)
                  "stop")))
    (if (string= state "play")
        (empdel--send-command "pause 1")
      (empdel--send-command "pause 0")))
  (empdel-refresh))

(defun empdel-next ()
  "Play next track."
  (interactive)
  (empdel--send-command "next")
  (empdel-refresh))

(defun empdel-previous ()
  "Play previous track."
  (interactive)
  (empdel--send-command "previous")
  (empdel-refresh))

(defun empdel-stop ()
  "Stop playback."
  (interactive)
  (empdel--send-command "stop")
  (empdel-refresh))

(defun empdel-play-selected ()
  "Play the track under point."
  (interactive)
  (let ((song-id (tabulated-list-get-id)))
    (when song-id
      (empdel--send-command (format "playid %s" song-id))
      (empdel-refresh))))

(defun empdel-toggle-consume ()
  "Toggle MPD consume mode (removes tracks from playlist after playing)."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (state (if (string-match "^consume: \\(1\\|0\\)$" status)
                    (match-string 1 status) "0")))
    (if (string= state "1")
        (progn (empdel--send-command "consume 0") (message "Consume mode: OFF"))
      (progn (empdel--send-command "consume 1") (message "Consume mode: ON")))))

(defun empdel-toggle-repeat ()
  "Toggle MPD repeat mode."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (state (if (string-match "^repeat: \\(1\\|0\\)$" status)
                    (match-string 1 status) "0")))
    (if (string= state "1")
        (progn (empdel--send-command "repeat 0") (message "Repeat mode: OFF"))
      (progn (empdel--send-command "repeat 1") (message "Repeat mode: ON")))))

(defun empdel-toggle-random ()
  "Toggle MPD random (shuffle) mode."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (state (if (string-match "^random: \\(1\\|0\\)$" status)
                    (match-string 1 status) "0")))
    (if (string= state "1")
        (progn (empdel--send-command "random 0") (message "Random mode: OFF"))
      (progn (empdel--send-command "random 1") (message "Random mode: ON")))))

(defun empdel-toggle-single ()
  "Toggle MPD single mode (stop after current track)."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (state (if (string-match "^single: \\(1\\|0\\)$" status)
                    (match-string 1 status) "0")))
    (if (string= state "1")
        (progn (empdel--send-command "single 0") (message "Single mode: OFF"))
      (progn (empdel--send-command "single 1") (message "Single mode: ON")))))

;;; --- UI & Mode Definition (View 1: Playlist) ---

(defvar empdel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'empdel-toggle-play)
    (define-key map (kbd "RET") 'empdel-play-selected)
    (define-key map (kbd ">") 'empdel-next)
    (define-key map (kbd "<") 'empdel-previous)
    (define-key map (kbd "d") 'empdel-playlist-delete)
    (define-key map (kbd "<delete>") 'empdel-playlist-delete)
    (define-key map (kbd "c") 'empdel-playlist-clear)
    (define-key map (kbd "g") 'empdel-refresh)
    (define-key map (kbd "s") 'empdel-playlists-save)
	
	(define-key map (kbd "C") 'empdel-toggle-consume)
    (define-key map (kbd "P") 'empdel-toggle-repeat)
    (define-key map (kbd "R") 'empdel-toggle-random)
    (define-key map (kbd "S") 'empdel-toggle-single)	
    ;; View switching
    (define-key map (kbd "1") 'empdel-view-playlist)
    (define-key map (kbd "2") 'empdel-view-browser)
    (define-key map (kbd "3") 'empdel-view-search)
    (define-key map (kbd "4") 'empdel-view-library)    
    (define-key map (kbd "5") 'empdel-view-playlists)
    map)
  "Keymap for `empdel-mode`.")

(define-derived-mode empdel-mode tabulated-list-mode "Empdel"
  "Major mode for browsing and controlling MPD, inspired by ncmpcpp."
  (setq tabulated-list-format
        [("Artist" 25 t)
         ("Title" 40 t)
         ("Album" 30 t)
         ("Time" 6 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun empdel-playlist-delete ()
  "Delete the selected song from the current playlist."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (empdel--send-command (format "deleteid %s" id))
      (empdel-refresh))))

(defun empdel-playlist-clear ()
  "Clear the entire current playlist."
  (interactive)
  (when (y-or-n-p "Clear current playlist? ")
    (empdel--send-command "clear")
    (empdel-refresh)))

(defun empdel-refresh ()
  "Refresh the playlist buffer and highlight the playing track."
  (interactive)
  (let* ((status (empdel--send-command "status"))
         (playing-id (if (string-match "^songid: \\([0-9]+\\)$" status)
                         (match-string 1 status) nil))
         (raw-playlist (empdel--send-command "playlistinfo"))
         (playlist-data (empdel--parse-playlist raw-playlist))
         (entries nil))
    (dolist (song playlist-data)
      (let* ((id (cdr (assq 'id song)))
             (artist (or (cdr (assq 'artist song)) "Unknown Artist"))
             (title (or (cdr (assq 'title song)) (file-name-nondirectory (cdr (assq 'file song)))))
             (album (or (cdr (assq 'album song)) "Unknown Album"))
             (time (empdel--format-time (cdr (assq 'time song))))
             ;; Apply highlighting if this is the currently playing song
             (face (if (equal id playing-id) 'empdel-playing-song-face nil)))
        
        (push (list id (vector (propertize artist 'font-lock-face face)
                               (propertize title 'font-lock-face face)
                               (propertize album 'font-lock-face face)
                               (propertize time 'font-lock-face face))) 
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

;;; --- View Management ---

(defun empdel-view-playlist ()
  "Switch to the Playlist view (View 1)."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Playlist*")))
    (with-current-buffer buf
      (unless (eq major-mode 'empdel-mode)
        (empdel-mode))
      (empdel-refresh))
    (switch-to-buffer buf)))

(defun empdel-view-browser ()
  "Switch to the File Browser view (View 2)."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Browser*")))
    (with-current-buffer buf
      (unless (eq major-mode 'empdel-browser-mode)
        (empdel-browser-mode))
      (empdel-browser-refresh))
    (switch-to-buffer buf)))

;;; --- Browser View (View 2) ---

(defvar-local empdel-browser-current-path ""
  "The current directory path being browsed.")

(defun empdel--parse-lsinfo (response)
  "Parse MPD lsinfo RESPONSE into an alist of directories and files."
  (let ((lines (split-string response "\n" t))
        (entries nil)
        (current-entry nil))
    (dolist (line lines)
      (when (string-match "^\\(directory\\|file\\|playlist\\): \\(.*\\)$" line)
        (let ((type (match-string 1 line))
              (val (match-string 2 line)))
          (when current-entry
            (push (nreverse current-entry) entries))
          (setq current-entry (list (cons 'type type) (cons 'uri val))))))
    (when current-entry
      (push (nreverse current-entry) entries))
    (nreverse entries)))

(defun empdel--parent-dir (path)
  "Return the parent directory of PATH, or an empty string if at root."
  (let ((dir (file-name-directory (directory-file-name path))))
    (if dir (substring dir 0 -1) "")))

(defun empdel-browser-refresh ()
  "Refresh the browser buffer based on the current path."
  (interactive)
  (let* ((cmd (if (string= empdel-browser-current-path "")
                  "lsinfo"
                (format "lsinfo %s" (empdel--safe-quote empdel-browser-current-path))))
         (raw-data (empdel--send-command cmd))
         (parsed-data (empdel--parse-lsinfo raw-data))
         (entries nil))
    (unless (string= empdel-browser-current-path "")
      (push (list ".." (vector "[DIR]" "..")) entries))
    (dolist (item parsed-data)
      (let* ((type (cdr (assq 'type item)))
             (uri (cdr (assq 'uri item)))
             (name (file-name-nondirectory uri))
             (display-type (if (string= type "directory") "[DIR]" " ")))
        (push (list uri (vector display-type name)) entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defun empdel-browser-enter ()
  "Navigate into the directory under point, or add the file."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (if (string= id "..")
          (empdel-browser-up)
        (let* ((entry (assq id tabulated-list-entries))
               (type-str (aref (cadr entry) 0)))
          (if (string= type-str "[DIR]")
              (progn
                (setq empdel-browser-current-path id)
                (empdel-browser-refresh))
            (empdel-browser-add)))))))

(defun empdel-browser-up ()
  "Navigate up one directory."
  (interactive)
  (unless (string= empdel-browser-current-path "")
    (setq empdel-browser-current-path (empdel--parent-dir empdel-browser-current-path))
    (empdel-browser-refresh)))

(defun empdel-browser-add ()
  "Add the file or directory under point to the current playlist."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (string= id "..")))
      (empdel--send-command (format "add %s" (empdel--safe-quote id)))
      (message "Added: %s" id)
      (forward-line 1)))) 

(defvar empdel-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'empdel-browser-enter)
    (define-key map (kbd "^") 'empdel-browser-up)
    (define-key map (kbd "u") 'empdel-browser-up)
    (define-key map (kbd "a") 'empdel-browser-add)
    (define-key map (kbd "SPC") 'empdel-browser-add)
    (define-key map (kbd "g") 'empdel-browser-refresh)
    (define-key map (kbd "1") 'empdel-view-playlist)
    (define-key map (kbd "2") 'empdel-view-browser)
    (define-key map (kbd "3") 'empdel-view-search)
    (define-key map (kbd "4") 'empdel-view-library)
    (define-key map (kbd "5") 'empdel-view-playlists)
    map)
  "Keymap for `empdel-browser-mode`.")

(define-derived-mode empdel-browser-mode tabulated-list-mode "Empdel-Browse"
  "Major mode for browsing the MPD file library."
  (setq tabulated-list-format [("Type" 6 t) ("Name" 80 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;; --- Search View (View 3) ---

(defvar-local empdel-search-last-query nil
  "Stores the last executed search query to allow refreshing.")

(defun empdel-view-search ()
  "Switch to the Search view (View 3)."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Search*")))
    (with-current-buffer buf
      (unless (eq major-mode 'empdel-search-mode)
        (empdel-search-mode))
      (when (not empdel-search-last-query)
        (call-interactively 'empdel-search-query)))
    (switch-to-buffer buf)))

(defun empdel-search-query (type query)
  "Prompt the user for a search TYPE and QUERY, then fetch results."
  (interactive
   (list (completing-read "Search by: " '("any" "artist" "album" "title") nil t "any")
         (read-string "Search query: ")))
  (setq empdel-search-last-query (format "search %s %s" type (empdel--safe-quote query)))
  (empdel-search-refresh))

(defun empdel-search-refresh ()
  "Execute the last search query and update the buffer."
  (interactive)
  (if (not empdel-search-last-query)
      (message "No search query active. Press 's' or '/' to search.")
    (let* ((raw-data (empdel--send-command empdel-search-last-query))
           (parsed-data (empdel--parse-playlist raw-data)) 
           (entries nil))
      (dolist (song parsed-data)
        (let ((uri (cdr (assq 'file song)))
              (artist (or (cdr (assq 'artist song)) "Unknown"))
              (title (or (cdr (assq 'title song)) (file-name-nondirectory (cdr (assq 'file song)))))
              (album (or (cdr (assq 'album song)) "Unknown")))
          (push (list uri (vector artist title album)) entries)))
      (setq tabulated-list-entries (nreverse entries))
      (tabulated-list-print t))))

(defun empdel-search-add ()
  "Add the selected search result to the current playlist."
  (interactive)
  (let ((uri (tabulated-list-get-id)))
    (when uri
      (empdel--send-command (format "add %s" (empdel--safe-quote uri)))
      (message "Added: %s" uri)
      (forward-line 1))))

(defvar empdel-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'empdel-search-query)
    (define-key map (kbd "a") 'empdel-search-add)
    (define-key map (kbd "SPC") 'empdel-search-add)
    (define-key map (kbd "RET") 'empdel-search-add)
    (define-key map (kbd "g") 'empdel-search-refresh)
    (define-key map (kbd "1") 'empdel-view-playlist)
    (define-key map (kbd "2") 'empdel-view-browser)
    (define-key map (kbd "3") 'empdel-view-search)
    (define-key map (kbd "4") 'empdel-view-library)
    (define-key map (kbd "5") 'empdel-view-playlists)
    map)
  "Keymap for `empdel-search-mode`.")

(define-derived-mode empdel-search-mode tabulated-list-mode "Empdel-Search"
  "Major mode for searching the MPD library."
  (setq tabulated-list-format [("Artist" 25 t) ("Title" 40 t) ("Album" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;; --- Library View (View 4) ---

(defvar-local empdel-library-level 'artists
  "Current drill-down level in the library: 'artists, 'albums, or 'songs.")

(defvar-local empdel-library-current-artist nil
  "The artist currently being browsed in the library.")

(defvar-local empdel-library-current-album nil
  "The album currently being browsed in the library.")

(defun empdel-view-library ()
  "Switch to the Media Library view (View 4)."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Library*")))
    (with-current-buffer buf
      (unless (eq major-mode 'empdel-library-mode)
        (empdel-library-mode))
      (empdel-library-refresh))
    (switch-to-buffer buf)))

(defun empdel--parse-list (response key)
  "Parse a simple MPD list RESPONSE, extracting values for KEY."
  (let ((lines (split-string response "\n" t))
        (items nil))
    (dolist (line lines)
      (when (string-match (format "^%s: \\(.*\\)$" key) line)
        (push (match-string 1 line) items)))
    (nreverse items)))

(defun empdel-library-refresh ()
  "Refresh the library buffer based on the current drill-down level."
  (interactive)
  (pcase empdel-library-level
    ('artists
     (setq tabulated-list-format [("Artist" 80 t)])
     (tabulated-list-init-header)
     (let* ((raw (empdel--send-command "list artist"))
            (artists (empdel--parse-list raw "Artist"))
            (entries nil))
       (dolist (artist artists)
         (unless (string= artist "")
           (push (list artist (vector artist)) entries)))
       (setq tabulated-list-entries (nreverse entries))))
    
    ('albums
     (setq tabulated-list-format [("Album" 80 t)])
     (tabulated-list-init-header)
     (let* ((cmd (format "list album artist %s" (empdel--safe-quote empdel-library-current-artist)))
            (raw (empdel--send-command cmd))
            (albums (empdel--parse-list raw "Album"))
            (entries (list (list ".." (vector "..")))))
       (dolist (album albums)
         (let ((display-name (if (string= album "") "Unknown Album" album)))
           (push (list album (vector display-name)) entries)))
       (setq tabulated-list-entries (nreverse entries))))
    
    ('songs
     (setq tabulated-list-format [("Track" 6 t) ("Title" 50 t) ("Time" 10 nil)])
     (tabulated-list-init-header)
     (let* ((cmd (format "find artist %s album %s" 
                         (empdel--safe-quote empdel-library-current-artist) 
                         (empdel--safe-quote empdel-library-current-album)))
            (raw (empdel--send-command cmd))
            (songs (empdel--parse-playlist raw))
            (entries (list (list ".." (vector "" ".." "")))))
       (dolist (song songs)
         (let ((uri (cdr (assq 'file song)))
               (track (or (cdr (assq 'track song)) ""))
               (title (or (cdr (assq 'title song)) (file-name-nondirectory (cdr (assq 'file song)))))
               (time (empdel--format-time (cdr (assq 'time song)))))
           (push (list uri (vector track title time)) entries)))
       (setq tabulated-list-entries (nreverse entries)))))
  (tabulated-list-print t))

(defun empdel-library-enter ()
  "Drill down into the selected artist or album."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (if (string= id "..")
          (empdel-library-up)
        (pcase empdel-library-level
          ('artists
           (setq empdel-library-current-artist id)
           (setq empdel-library-level 'albums)
           (empdel-library-refresh))
          ('albums
           (setq empdel-library-current-album id)
           (setq empdel-library-level 'songs)
           (empdel-library-refresh))
          ('songs
           (empdel-library-add)))))))

(defun empdel-library-up ()
  "Go up one level in the media library."
  (interactive)
  (pcase empdel-library-level
    ('songs
     (setq empdel-library-level 'albums)
     (empdel-library-refresh))
    ('albums
     (setq empdel-library-level 'artists)
     (empdel-library-refresh))))

(defun empdel-library-add ()
  "Add the selected artist, album, or song to the playlist."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (string= id "..")))
      (pcase empdel-library-level
        ('artists
         (empdel--send-command (format "findadd artist %s" (empdel--safe-quote id))))
        ('albums
         (empdel--send-command (format "findadd artist %s album %s" 
                                       (empdel--safe-quote empdel-library-current-artist) 
                                       (empdel--safe-quote id))))
        ('songs
         (empdel--send-command (format "add %s" (empdel--safe-quote id)))))
      (message "Added: %s" id)
      (forward-line 1))))

(defvar empdel-library-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'empdel-library-enter)
    (define-key map (kbd "^") 'empdel-library-up)
    (define-key map (kbd "u") 'empdel-library-up)
    (define-key map (kbd "a") 'empdel-library-add)
    (define-key map (kbd "SPC") 'empdel-library-add)
    (define-key map (kbd "g") 'empdel-library-refresh)
    (define-key map (kbd "1") 'empdel-view-playlist)
    (define-key map (kbd "2") 'empdel-view-browser)
    (define-key map (kbd "3") 'empdel-view-search)
    (define-key map (kbd "4") 'empdel-view-library)
    (define-key map (kbd "5") 'empdel-view-playlists)
    map)
  "Keymap for `empdel-library-mode`.")

(define-derived-mode empdel-library-mode tabulated-list-mode "Empdel-Library"
  "Major mode for browsing the MPD media library.")

;;; --- Playlist Editor View (View 5) ---

(defvar-local empdel-playlists-level 'playlists
  "Current level in the playlist editor: 'playlists or 'songs.")

(defvar-local empdel-playlists-current-playlist nil
  "The saved playlist currently being browsed.")

(defun empdel-view-playlists ()
  "Switch to the Playlist Editor view (View 5)."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Playlists*")))
    (with-current-buffer buf
      (unless (eq major-mode 'empdel-playlists-mode)
        (empdel-playlists-mode))
      (setq empdel-playlists-level 'playlists)
      (empdel-playlists-refresh))
    (switch-to-buffer buf)))

(defun empdel--parse-playlists-list (response)
  "Parse MPD listplaylists RESPONSE."
  (let ((lines (split-string response "\n" t))
        (playlists nil))
    (dolist (line lines)
      (when (string-match "^playlist: \\(.*\\)$" line)
        (push (match-string 1 line) playlists)))
    (nreverse playlists)))

(defun empdel-playlists-refresh ()
  "Refresh the playlist editor buffer based on the current level."
  (interactive)
  (pcase empdel-playlists-level
    ('playlists
     (setq tabulated-list-format [("Playlist Name" 80 t)])
     (tabulated-list-init-header)
     (let* ((raw (empdel--send-command "listplaylists"))
            (playlists (empdel--parse-playlists-list raw))
            (entries nil))
       (dolist (pl playlists)
         (push (list pl (vector pl)) entries))
       (setq tabulated-list-entries (nreverse entries))))
    
    ('songs
     (setq tabulated-list-format [("Artist" 25 t) ("Title" 40 t) ("Time" 10 nil)])
     (tabulated-list-init-header)
     (let* ((cmd (format "listplaylistinfo %s" (empdel--safe-quote empdel-playlists-current-playlist)))
            (raw (empdel--send-command cmd))
            (songs (empdel--parse-playlist raw))
            (entries (list (list ".." (vector "" ".." "")))))
       (dolist (song songs)
         (let ((uri (cdr (assq 'file song)))
               (artist (or (cdr (assq 'artist song)) "Unknown"))
               (title (or (cdr (assq 'title song)) (file-name-nondirectory (cdr (assq 'file song)))))
               (time (empdel--format-time (cdr (assq 'time song)))))
           (push (list uri (vector artist title time)) entries)))
       (setq tabulated-list-entries (nreverse entries)))))
  (tabulated-list-print t))

(defun empdel-playlists-enter ()
  "Drill down into the selected playlist, or add the selected song."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (if (string= id "..")
          (empdel-playlists-up)
        (pcase empdel-playlists-level
          ('playlists
           (setq empdel-playlists-current-playlist id)
           (setq empdel-playlists-level 'songs)
           (empdel-playlists-refresh))
          ('songs
           (empdel-playlists-add)))))))

(defun empdel-playlists-up ()
  "Go up to the list of playlists."
  (interactive)
  (pcase empdel-playlists-level
    ('songs
     (setq empdel-playlists-level 'playlists)
     (empdel-playlists-refresh))))

(defun empdel-playlists-add ()
  "Load the selected playlist or add the selected song to the current queue."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (string= id "..")))
      (let* ((cmd (pcase empdel-playlists-level
                    ('playlists (format "load %s" (empdel--safe-quote id)))
                    ('songs     (format "add %s" (empdel--safe-quote id)))))
             ;; Send the command and capture MPD's response
             (response (empdel--send-command cmd)))
        
        ;; Check if MPD returned an error (ACK)
        (if (string-match "^ACK \\(.*\\)" response)
            (message "MPD Error: Could not add (%s)" (match-string 1 response))
          ;; Success!
          (message "Added: %s" id)
          (forward-line 1))))))

(defun empdel-playlists-delete ()
  "Delete the selected saved playlist, handling quotes and MPD errors."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (eq empdel-playlists-level 'playlists))
      (when (y-or-n-p (format "Delete playlist '%s'? " id))
        (let* ((cmd (format "rm %s" (empdel--safe-quote id)))
               (response (empdel--send-command cmd)))
          (if (string-match "^ACK \\(.*\\)" response)
              (message "MPD Error: Could not delete (%s)" (match-string 1 response))
            (message "Deleted playlist: %s" id)
            (empdel-playlists-refresh)))))))

(defun empdel-playlists-save (name)
  "Save the current active queue as a new playlist."
  (interactive "sSave current playlist as: ")
  (if (string= name "")
      (message "Playlist name cannot be empty.")
    (empdel--send-command (format "save %s" (empdel--safe-quote name)))
    (message "Saved current queue as: %s" name)
    (when (and (eq major-mode 'empdel-playlists-mode)
               (eq empdel-playlists-level 'playlists))
      (empdel-playlists-refresh))))

(defvar empdel-playlists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'empdel-playlists-enter)
    (define-key map (kbd "^") 'empdel-playlists-up)
    (define-key map (kbd "u") 'empdel-playlists-up)
    (define-key map (kbd "a") 'empdel-playlists-add)
    (define-key map (kbd "SPC") 'empdel-playlists-add)
    (define-key map (kbd "d") 'empdel-playlists-delete)
    (define-key map (kbd "g") 'empdel-playlists-refresh)
    (define-key map (kbd "s") 'empdel-playlists-save)
    (define-key map (kbd "1") 'empdel-view-playlist)
    (define-key map (kbd "2") 'empdel-view-browser)
    (define-key map (kbd "3") 'empdel-view-search)
    (define-key map (kbd "4") 'empdel-view-library)
    (define-key map (kbd "5") 'empdel-view-playlists)
    map)
  "Keymap for `empdel-playlists-mode`.")

(define-derived-mode empdel-playlists-mode tabulated-list-mode "Empdel-Playlists"
  "Major mode for managing saved MPD playlists.")

;;;###autoload
(defun empdel ()
  "Launch the Empdel MPD client."
  (interactive)
  (let ((buf (get-buffer-create "*Empdel Playlist*")))
    (with-current-buffer buf
      (empdel-mode)
      (empdel-refresh))
    (switch-to-buffer buf)))

(provide 'empdel)
;;; empdel.el ends here
