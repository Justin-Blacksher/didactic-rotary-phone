;;; nes-asm.el --- NES 6502 Assembly major mode with graphics and chiptune support -*- lexical-binding: t -*-
;;; Commentary:
;; Major mode for NES 6502 assembly with BMP sprite editing and APU chiptune composition.
;; Supports syntax highlighting, 8x8 sprite editing (2-bit BMP), and chiptune sequences with duty cycles.
;; Designed for West Coast gangsta chiptune vibes. Derives from asm-mode, compatible with nasm-mode.
;;; Code:
;; Ensure cl-lib is available for loops and mapping
(require 'cl-lib)
;; Ensure asm-mode is loaded as the base mode
(unless (featurep 'asm-mode)
 (require 'asm-mode))
;;; Major Mode: nes-asm-mode
(defvar nes-asm-font-lock-keywords
 `(("\\<\\(ADC\\|AND\\|ASL\\|BCC\\|BCS\\|BEQ\\|BIT\\|BMI\\|BNE\\|BPL\\|BRK\\|BVC\\|BVS\\|CLC\\|CLD\\|CLI\\|CLV\\|CMP\\|CPX\\|CPY\\|DEC\\|DEX\\|DEY\\|EOR\\|INC\\|INX\\|INY\\|JMP\\|JSR\\|LDA\\|LDX\\|LDY\\|LSR\\|NOP\\|ORA\\|PHA\\|PHP\\|PLA\\|PLP\\|ROL\\|ROR\\|RTI\\|RTS\\|SBC\\|SEC\\|SED\\|SEI\\|STA\\|STX\\|STY\\|TAX\\|TAY\\|TSX\\|TXA\\|TXS\\|TYA\\)\\>" . font-lock-keyword-face)
   ("\\$[0-9A-Fa-f]+" . font-lock-constant-face)
   ("\\.[a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   ("^[a-zA-Z0-9_]+:" . font-lock-function-name-face)
   (";[^\n]*" . font-lock-comment-face)))
(defun nes-asm-indent-line ()
 "Indent current line for NES assembly."
 (let ((indent (cond ((looking-at "^[a-zA-Z0-9_]+:") 0)
                     ((looking-at "^\\s-*;") 0)
                     (t 2))))
   (indent-line-to indent)))
(define-derived-mode nes-asm-mode asm-mode "NES-Asm"
 "Major mode for editing NES 6502 assembly code."
 :group 'nes-asm
 (unless (eq major-mode 'nes-asm-mode)  ; Avoid redefinition issues
   (setq font-lock-defaults '(nes-asm-font-lock-keywords)))
 (setq-local comment-start ";")
 (setq-local comment-start-skip ";+\\s-*")
 (setq-local indent-line-function 'nes-asm-indent-line)
 (setq-local imenu-generic-expression
             '(("Labels" "^\\([a-zA-Z0-9_]+\\):" 1)))
 (imenu-add-to-menubar "Labels"))
;;; Minor Mode: nes-asm-graphics-mode (BMP Sprite Editor)
(defvar nes-asm-current-palette '("1F1122" "FFCC00" "555555" "FFFFFF")
 "Current NES 4-color palette (hex RGB, default: gangsta purple/gold/gray/white).")
(defvar nes-asm-sprite-data nil
 "Current sprite data as 8x8 pixel grid (2-bit values).")
(defun nes-asm-parse-bmp (filename)
 "Parse a BMP file for an 8x8 NES sprite."
 (with-temp-buffer
   (insert-file-contents filename)
   (goto-char (point-min))
   (unless (and (= (char-after) ?B) (= (char-after (1+ (point))) ?M))
     (error "Not a valid BMP file"))
   (goto-char 10) ; Offset to pixel data
   (let ((offset (read (current-buffer))))
     (goto-char 18) ; Width
     (unless (= (read (current-buffer)) 8)
       (error "BMP must be 8x8 pixels"))
     (goto-char 22) ; Height
     (unless (= (read (current-buffer)) 8)
       (error "BMP must be 8x8 pixels"))
     (goto-char 28) ; Bits per pixel
     (unless (= (read (current-buffer)) 2)
       (error "BMP must be 2-bit (4 colors)"))
     (goto-char offset)
     (let ((pixels (make-vector 64 0)))
       (dotimes (y 8)
         (dotimes (x 8)
           (aset pixels (+ (* (- 7 y) 8) x) (logand (char-after) #x03))
           (forward-char)))
       (setq nes-asm-sprite-data pixels)))))
(defun nes-asm-open-sprite-editor (filename)
 "Open a buffer for editing NES sprites from a BMP file."
 (interactive "fBMP file: ")
 (nes-asm-parse-bmp filename)
 (let ((buffer (generate-new-buffer "*NES-Sprite-Editor*")))
   (with-current-buffer buffer
     (nes-asm-graphics-mode 1)
     (insert ";; NES Sprite Editor (8x8, 2-bit, use C-c p to change palette, C-c e to export)\n")
     (nes-asm-render-sprite-buffer)
     (nes-asm-render-sprite-preview))
   (pop-to-buffer buffer)))
(defun nes-asm-render-sprite-buffer ()
 "Render sprite data as text in the editor buffer."
 (let ((inhibit-read-only t))
   (erase-buffer)
   (insert ";; NES Sprite Editor (8x8, 2-bit)\n")
   (dotimes (y 8)
     (dotimes (x 8)
       (insert (format "%d " (aref nes-asm-sprite-data (+ (* (- 7 y) 8) x)))))
     (insert "\n"))
   (goto-char (point-min))))
(defun nes-asm-render-sprite-preview ()
 "Render a preview of the current sprite in a side buffer."
 (let ((preview-buffer (get-buffer-create "*NES-Sprite-Preview*")))
   (with-current-buffer preview-buffer
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert "Sprite Preview (8x8, Palette: "
               (mapconcat 'identity nes-asm-current-palette ", ") ")\n")
       (dotimes (y 8)
         (dotimes (x 8)
           (let ((color (nth (aref nes-asm-sprite-data (+ (* (- 7 y) 8) x))
                             nes-asm-current-palette)))
             (insert (propertize "█" 'face `(:background ,color)))))
         (insert "\n"))
       (read-only-mode 1)))
   (display-buffer preview-buffer)))
(defun nes-asm-set-palette ()
 "Set a new NES 4-color palette."
 (interactive)
 (let ((new-palette (split-string
                     (read-string "Enter 4 hex colors (e.g., 1F1122 FFCC00 555555 FFFFFF): "))))
   (if (= (length new-palette) 4)
       (progn
         (setq nes-asm-current-palette new-palette)
         (nes-asm-render-sprite-preview))
     (message "Please provide exactly 4 hex colors."))))
(defun nes-asm-export-chr ()
 "Export sprite to NES CHR format."
 (interactive)
 (let ((chr-data (make-vector 16 0)))
   (dotimes (y 8)
     (dotimes (x 8)
       (let* ((pixel (aref nes-asm-sprite-data (+ (* y 8) x)))
              (bit0 (logand pixel 1))
              (bit1 (logand (ash pixel -1) 1))
              (offset (* y 2)))
         (aset chr-data offset
               (logior (aref chr-data offset)
                       (ash bit0 (- 7 x))))
         (aset chr-data (1+ offset)
               (logior (aref chr-data (1+ offset))
                       (ash bit1 (- 7 x)))))))
   (with-temp-file "sprite.chr"
     (dotimes (i 16)
       (insert (char-to-string (aref chr-data i)))))
   (message "Exported sprite to sprite.chr")))
(defvar nes-asm-graphics-mode-map
 (let ((map (make-sparse-keymap)))
   (define-key map (kbd "C-c g") 'nes-asm-open-sprite-editor)
   (define-key map (kbd "C-c p") 'nes-asm-set-palette)
   (define-key map (kbd "C-c e") 'nes-asm-export-chr)
   map))
(define-minor-mode nes-asm-graphics-mode
 "Minor mode for NES sprite editing."
 :lighter " NES-Gfx"
 :keymap nes-asm-graphics-mode-map
 (if nes-asm-graphics-mode
     (message "NES Graphics Mode enabled")
   (message "NES Graphics Mode disabled")))
;;; Minor Mode: nes-asm-apu-mode (Chiptune Synth with Duty Cycle)
(defvar nes-asm-apu-sequences nil
 "List of chiptune sequences (square1, square2, triangle, noise).")
(defvar nes-asm-apu-note-table
 '((:C4 . 0x0D) (:D4 . 0x0F) (:E4 . 0x11) (:F4 . 0x12) (:G4 . 0x14) (:A4 . 0x16) (:B4 . 0x18))
 "NES APU note frequencies (simplified).")
(defvar nes-asm-apu-duty-table
 '((12 . 0x00) (25 . 0x40) (50 . 0x80) (75 . 0xC0))
 "NES APU duty cycles for square waves (12%, 25%, 50%, 75%).")
(defun nes-asm-apu-open-synth ()
 "Open a buffer for composing NES chiptunes."
 (interactive)
 (let ((buffer (generate-new-buffer "*NES-APU-Synth*")))
   (with-current-buffer buffer
     (nes-asm-apu-mode 1)
     (insert ";; NES APU Synth (C4-B4 notes, format: square1: C4:25 D4:50 | square2: E4 F4 | ...)\n")
     (insert "square1: E4:25 F4:25 G4:25 F4:25\n") ; G-funk lead
     (insert "square2: A4 G4 F4 G4\n")            ; Smooth harmony
     (insert "triangle: D4 D4 D4 D4\n")           ; Thumpin' bass
     (insert "noise: C4 _ E4 _\n")                ; Snappy snare with swing
     (nes-asm-apu-render-preview))
   (pop-to-buffer buffer)))
(defun nes-asm-apu-parse-sequences ()
 "Parse chiptune sequences from the current buffer, including duty cycles."
 (interactive)
 (setq nes-asm-apu-sequences nil)
 (goto-char (point-min))
 (while (re-search-forward "^\\(square1\\|square2\\|triangle\\|noise\\):\\s-*\\(.+\\)$" nil t)
   (let ((channel (intern (match-string 1)))
         (notes (mapcar (lambda (n)
                          (if (string= n "_")
                              '(:rest . nil)
                            (let ((parts (split-string n ":")))
                              (list (intern (car parts))
                                    (if (cadr parts) (string-to-number (cadr parts)) nil)))))
                        (split-string (match-string 2)))))
     (push (cons channel notes) nes-asm-apu-sequences)))
 (nes-asm-apu-render-preview)
 (message "Parsed chiptune sequences"))
(defun nes-asm-apu-render-preview ()
 "Simulate playback of chiptune sequences."
 (let ((preview-buffer (get-buffer-create "*NES-APU-Preview*")))
   (with-current-buffer preview-buffer
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert "Chiptune Preview\n")
       (dolist (seq nes-asm-apu-sequences)
         (insert (format "%s: %s\n" (car seq)
                         (mapconcat (lambda (n)
                                      (if (eq (car n) :rest)
                                          "_"
                                        (format "%s%s" (car n)
                                                (if (cadr n) (format ":%d" (cadr n)) ""))))
                                    (cdr seq) " "))))
       (read-only-mode 1)))
   (display-buffer preview-buffer)))
(defun nes-asm-apu-export ()
 "Export chiptune to NES data with duty cycles."
 (interactive)
 (with-temp-file "music.asm"
   (insert ";; NES APU Music Data\n")
   (dolist (seq nes-asm-apu-sequences)
     (let ((channel (car seq))
           (notes (cdr seq)))
       (insert (format "%s_data:\n" channel))
       (dolist (note notes)
         (if (eq (car note) :rest)
             (insert ".db $FF ; Rest\n")
           (let ((freq (cdr (assoc (car note) nes-asm-apu-note-table)))
                 (duty (if (and (cadr note) (memq channel '(square1 square2)))
                           (cdr (assoc (cadr note) nes-asm-apu-duty-table))
                         0)))
             (insert (format ".db $%02X ; Note\n" (or freq 0)))
             (when duty
               (insert (format ".db $%02X ; Duty\n" duty))))))
       (insert ".db $00\n"))))
 (message "Exported chiptune to music.asm"))
(defvar nes-asm-apu-mode-map
 (let ((map (make-sparse-keymap)))
   (define-key map (kbd "C-c s") 'nes-asm-apu-parse-sequences)
   (define-key map (kbd "C-c e") 'nes-asm-apu-export)
   map))
(define-minor-mode nes-asm-apu-mode
 "Minor mode for NES APU chiptune composition."
 :lighter " NES-APU"
 :keymap nes-asm-apu-mode-map
 (if nes-asm-apu-mode
     (message "NES APU Synth Mode enabled")
   (message "NES APU Synth Mode disabled")))
;;; Compilation and Emulator Integration
(defun nes-asm-compile-and-run ()
 "Compile NES assembly and run in emulator."
 (interactive)
 (compile "ca65 %s && ld65 -o output.nes")
 (shell-command "mesen output.nes"))
(define-key nes-asm-mode-map (kbd "C-c C-c") 'nes-asm-compile-and-run)
;;; File Associations
(add-to-list 'auto-mode-alist '("\\.nesasm\\'" . nes-asm-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nes-asm-mode))  ; Override for NES files
(provide 'nes-asm-mode)
;;; nes-asm.el ends here
