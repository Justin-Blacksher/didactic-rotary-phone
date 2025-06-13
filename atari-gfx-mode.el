;;; package --- Summary
;;; Commentary:
;;; Code:

(define-minor-mode atari-gfx-mode
  "Minor mode for visualizing Atari 8-bit graphics data."
  :lighter " AtariGFX"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") 'atari-preview-bitmap)
            (define-key map (kbd "C-c c") 'atari-set-color)
            map))

(defun atari-highlight-bits ()
  "Highlight bit sequences in Atari 8-bit graphics mode."
  (interactive)
  (font-lock-add-keywords nil
                          '(("0" . font-lock-comment-face)
                            ("1" . font-lock-keyword-face)))
  (font-lock-flush))

(define-key atari-gfx-mode-map (kbd "C-c b") 'atari-highlight-bits)

(defvar atari-color-map
  '(("00" . "Black")
    ("01" . "Dark Blue")
    ("10" . "Red")
    ("11" . "White"))
  "Color map for Atari 8-bit graphics.")

(defun atari-set-color (bitpair color)
  "Modify the COLOR associated with a BITPAIR."
  (interactive "sBit pair (00/01/10/11): \nsNew color: ")
  (setq atari-color-map (assoc-delete-all bitpair atari-color-map))
  (add-to-list 'atari-color-map (cons bitpair color)))

(define-key atari-gfx-mode-map (kbd "C-c c") 'atari-set-color)


(defun atari-toggle-bit (pos)
  "Toggle is a bit at a given position in the current line.  POS."
  (interactive "nBit position (0-7): ")
  (save-excursion
    (beginning-of-line)
    (let* ((current-char (thing-at-point 'word))
           (binary (if (string-match "^[01]+$" current-char) current-char nil)))
      (if binary
          (let* ((new-bit (if (equal (substring binary pos (1+ pos)) "0") "1" "0"))
                 (modified (concat (substring binary 0 pos) new-bit (substring binary (1+ pos)))))
            (delete-region (line-beginning-position) (line-end-position))
            (insert modified))
        (message "No binary sequence found at cursor!")))))

(define-key atari-gfx-mode-map (kbd "C-c t") 'atari-toggle-bit)

(defun atari-preview-bitmap ()
  "Render a visual preview of Atari bitmap data in the current buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Atari Bitmap Preview*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[01]+$" nil t)
          (let* ((binary (match-string 0))
                 (visual (replace-regexp-in-string "[01]"
                                                   (lambda (bit) (if (string= bit "1") "#" "."))
                                                   binary)))
            (insert visual "\n")))))
    (display-buffer output-buffer)))

    
(define-key atari-gfx-mode-map (kbd "C-c p") 'atari-preview-bitmap)


(defvar atari-frame-list nil "List of frames for animation.")
(defvar atari-current-frame 0 "Index of the current animation frame.")
(defun atari-gfx-next-frame ()
 "Cycle through stored Atari bitmap frames."
 (interactive)
 (if atari-frame-list
     (let ((frame (nth atari-current-frame atari-frame-list)))
       (erase-buffer)
       (insert frame)
       (setq atari-current-frame (mod (1+ atari-current-frame) (length atari-frame-list))))
   (message "No frames stored!")))
(defun atari-gfx-play-animation ()
 "Automatically cycle through stored frames."
 (interactive)
 (run-with-timer 0.5 0.5 'atari-gfx-next-frame))

(defun atari-hex-to-binary (hex)
 "Convert HEX to 8-bit binary."
 (interactive "sHex value (00-FF): ")
 (message "Binary: %s" (format "%08b" (string-to-number hex 16))))
(defun atari-binary-to-hex (binary)
 "Convert BINARY to hex."
 (interactive "sBinary value (00000000-11111111): ")
 (message "Hex: %s" (format "%02X" (string-to-number binary 2))))

(defvar atari-color-map
 '(("00" . "Black") ("01" . "Dark Blue")
   ("10" . "Red") ("11" . "White"))
 "Color map for Atari graphics.")


(provide 'atari-gfx-mode)
;;; atari-gfx-mode.el ends here
