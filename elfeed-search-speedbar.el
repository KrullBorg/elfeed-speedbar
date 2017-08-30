;;; elfeed-speedbar --- Speedbar support for elfeed

;;; Commentary:
;;
;; Speedbar provides a frame in which files, and locations in files
;; are displayed.  These functions provide elfeed specific support,
;; showing feeds list in the side-bar.
;;
;;   This file requires speedbar.

;;; Code:

(require 'elfeed)
(require 'speedbar)

(defvar elfeed-search-speedbar-key-map nil
  "Keymap used when in elfeed display mode.")
(defvar elfeed-search-speedbar-menu-items nil
  "Additional menu-items to add to speedbar frame.")

(defun elfeed-search-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance elfeed."
  (setq elfeed-search-speedbar-key-map (speedbar-make-specialized-keymap))
  (define-key elfeed-search-speedbar-key-map "RET" 'speedbar-edit-line)
  (define-key elfeed-search-speedbar-key-map "e" 'speedbar-edit-line))

(if (featurep 'speedbar)
	(elfeed-search-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'elfeed-search-install-speedbar-variables))

(defun elfeed~speedbar-render-feeds-list ()
  "Insert the list of feeds in the speedbar."
  (interactive)
  (setq mylist (hash-table-keys elfeed-db-feeds))
  (setq titles (list))
  (dolist (feed mylist)
	(setq feed-title (plist-get (elfeed-feed-meta (elfeed-db-get-feed feed)) :title))
	(add-to-list 'titles feed-title))
  (setq titles-sorted (sort titles #'string-lessp))
  (dolist (feed-title titles-sorted)
	(speedbar-insert-button
	 (concat "  " feed-title)
	 'italic
	 'highlight
	 'elfeed~speedbar-feed
	 feed-title)))

;;;###autoload
(defun elfeed-search-speedbar-buttons (buffer)
  "Create buttons for any elfeed BUFFER."
  (interactive)
  (erase-buffer)
  (insert (propertize "* elfeed\n\n" 'face 'italic))

  (insert (propertize " Bookmarks\n" 'face 'italic))
  ;;;(mu4e~speedbar-render-bookmark-list)
  (insert "\n")
  (insert (propertize " Feeds\n" 'face 'italic))
  (elfeed~speedbar-render-feeds-list)
  )

(provide 'elfeed-search-speedbar)
;;; elfeed-search-speedbar.el ends here
