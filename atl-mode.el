(require 'lsp-mode)
(defvar atl-mode-hook nil)

(defun atl-command-at-point (command)
  (lsp--send-execute-command command (lsp--text-document-position-params)))

(defun atl-refine-at-point ()
  "Refine the hole at point"
  (interactive)
  (atl-command-at-point "refine"))

(defun atl-case-split-at-point ()
  "Case split the hole comment at point"
  (interactive)
  (atl-command-at-point "case-split"))

(defvar atl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-r" 'atl-refine-at-point)
    (define-key map "\C-c\C-c" 'atl-case-split-at-point)
    map)
  "Keymap for ATL major mode")

(add-to-list 'auto-mode-alist '("\\.atl\\'" . atl-mode))

(regexp-opt '("type" "prop"))

(defconst atl-font-lock-keywords
  (list
   '("\\b\\(?:prop\\|type\\|string\\)\\b" . font-lock-builtin-face)
   '("\\b\\(?:Definition\\|ValidationTest\\|UnitTest\\|fn\\|match\\|with\\|in\\|record\\|open\\|enum\\|Module\\|Require\\|let\\|optional\\|list\\set\\field-access\\|enum-cons\\|enum-tail\\|enum-head\\|as\\|annotate\\)\\b" . font-lock-keyword-face)
   '("(\\b[a-zA-Z][a-zA-Z0-9\\-]*)|('([^'](\\')*)+')" . font-lock-variable-name-face))
  "Minimal highlighting expressions for ATL mode")

(defvar atl-mode-syntax-table
  (let ((atl-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?- "w" atl-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124" atl-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" atl-mode-syntax-table)
    (modify-syntax-entry ?\n ">" atl-mode-syntax-table)
    atl-mode-syntax-table
    )
  "Syntax table for ATL mode")

(defcustom atl-lsp-jar "" "The path of the ATL LSP jar file" :type 'file)
(defcustom atl-lsp-args () "Additional arguments to pass to ATL LSP server" :type '(repeat string))

(define-derived-mode atl-mode fundamental-mode "ATL"
  "Major mode for editing ATL files"
  (set (make-local-variable 'font-lock-defaults) '(atl-font-lock-keywords))
  (if (null (assoc 'atl-mode hs-special-modes-alist))
      (add-to-list 'hs-special-modes-alist
	     '(atl-mode "{" "}" "/[*/]" nil nil)))
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (setq indent-tabs-mode nil)
  (add-to-list 'lsp-language-id-configuration '(atl-mode . "atl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda () (append (list "java" "-jar" atl-lsp-jar) atl-lsp-args)))
                    :major-modes '(atl-mode)
                    :server-id 'atl-lsp))
  (lsp))
