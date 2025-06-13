;;; Package --- Summary
;;; Commentary:
;;; Code:
(define-derived-mode atari8bit-mode prog-mode "Atari8Bit"
  "Major mode for editing Atari 8-bit assembly code."

  (defvar atari-asm-font-lock-keywords nil
    "Syntax highlighting keywords for Atari 8-bit assembly mode.")
 
  
  ;; Syntax highlighting
  (setq atari-asm-font-lock-keywords
        `((,(regexp-opt '("LDA" "STA" "JSR" "RTS" "LDX" "LDY" "STX" "STY" "ADC" "SBC"
                          "AND" "ORA" "EOR" "CMP" "CPX" "CPY" "DEC" "INC" "ASL" "LSR"
                          "ROL" "ROR" "BCC" "BCS" "BEQ" "BNE" "BMI" "BPL" "BRK" "NOP"
                          "SEC" "SED" "SEI" "CLC" "CLD" "CLI" "PHA" "PLA" "PHP" "PLP"
                          "TAX" "TXA" "DEX" "INX" "TAY" "TYA" "DEY" "INY" "TXS" "TSX"
                          "BIT") 'words) . font-lock-keyword-face)

          ;; Registers A, X, Y
          ("\\b[AXY]\\b" . font-lock-variable-name-face)
          ;; Hardware addresses ($D40A, etc.)
          ("\\$[0-0A-Fa-f]+" . font-lock-constant-face)))

  (setq-local font-lock-defaults '(atari-asm-font-lock-keywords)))

(defun atari-asm-indent-line ()
  "Indent current line for Atari 8-bit assembly."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (atari-asm-calculate-indent)))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun atari-asm-calculate-indent ()
  "Determine the indentation level for Atari assembly."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Labels start at column 0
     ((looking-at "^[A-Za-z_]+:") 0)
     ;; Opcodes align at column 4
     ((looking-at "^[ \t]*\\b\LDA\\|STA\\|JSR\\|RTS\\|LDX\\|LDY\\|STX\\|STY\\\b") 4)
     ;; Default: keep previous indent
     (t (current-indentation)))))

(defun atari-asm-insert-template ()
  "Insert a basic Atari 8-bit assembly template."
  (interactive)
  (insert ".ORG $2000\n")
  (insert "MVSC = $0058\n")
  (insert ";; Data\n"))


;; Add keybindings
(defvar atari-asm-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Atari 8-bit assembly mode.")

(define-key atari-asm-mode-map (kbd "C-c m") 'atari-lookup)
(define-key atari-asm-mode-map (kbd "C-c t") 'atari-asm-insert-template)
(define-key atari-asm-mode-map (kbd "C-c r") 'revert-buffer)

(defvar atari-memory-map
  '(("$D40A" . "Sound Control Register")
    ("$D000-$DFFF" . "ANTIC Graphics Registers")
    ("$A000-$BFFF" . "Basic ROM")
    ("$8000-$9FFF" . "Cartridge ROM")
    ("$D200" . "Pokey Sound Chip")
    ("$D300" . "GTIA Graphics Chip")
    ("$D500" . "PIA (Peripheral Interface Adapter")
    ("$D700" . "Math Pack Floating Point Routines"))
  "Atari 8-Bit memory addresses and descriptions.")

(defun atari-lookup (address)
  "Lookup an Atari Memory ADDRESS."
  (interactive "sEnter address (e.g., $D40A: ")
  (let ((description (cdr (assoc address atari-memory-map))))
    (if description
        (message "%s -> %s" address description)
      (message "Address not found"))))


  

(setq-local indent-line-function 'atari-asm-indent-line)

(provide 'atari8bit-mode)

;;; atari8bit-mode.el ends here
