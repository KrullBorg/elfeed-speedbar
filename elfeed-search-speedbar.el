;;; elfeed-speedbar --- Speedbar support for elfeed

;; Copyright (C) 2017 Andrea Zagli <azagli@libero.it>
;;
;; Author: Andrea Zagli <azagli@libero.it>
;; Version: 0.1
;; Keywords: feeds, tags, tools, speedbar, elfeed
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defun elfeed~speedbar-feed (&optional text token ident)
  "Filter news by TOKEN. TEXT and INDENT are not used."
  (speedbar-with-attached-buffer (elfeed-search-set-filter (concat elfeed-search-filter " " token))))

;;;###autoload
(defun elfeed-search-speedbar-buttons (buffer)
  "Create buttons for any elfeed BUFFER."
  (interactive)
  (erase-buffer)
  (insert (propertize "* elfeed\n\n" 'face 'italic))

  (insert (propertize " Feeds\n" 'face 'italic))
  (elfeed~speedbar-render-feeds-list)
  )

(provide 'elfeed-search-speedbar)
;;; elfeed-search-speedbar.el ends here
