(require 'package)
(require 'use-package)

;;
;; Package archives
;;
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(package-initialize)

;;
;; Loads paths, exec paths, etc.
;;
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)
  (add-to-list 'load-path "~/.emacs.d/lisp/"))
(setq custom-theme-directory "~/.emacs.d/themes")
(setq exec-path (append exec-path '("/Users/dmatysiak/bin" "/usr/local/bin")))

;;
;; File versioning
;;
(setq backup-by-copying t)   ; don't clobber symlinks
(setq version-control t)     ; use versioned backups
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 2)

(setq inhibit-startup-message t)
(setq column-number-mode t)
(windmove-default-keybindings)
(global-auto-revert-mode 1)
;;(global-undo-tree-mode)
;;(helm-cider-mode 1)

;;
;; MacOS-specific
;;
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
;; (when (eq system-type 'darwin)
(setq mac-option-modifier 'meta)

;;
;; Editor and UI
;;
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq-default fill-column 90)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tooltip-mode nil)
(fringe-mode '(0 . 0))
(show-paren-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;;
;; Mouse and scrolling
;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq maximum-scroll-margin 0.1)
(setq scroll-margin 99999)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 1)

;;
;; Before save hooks
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'font-lock-fontify-buffer)
(add-hook 'after-init-hook 'global-company-mode)

;;
;; Tools
;;
(use-package magit
  :ensure t
  :bind ("M-g" . magit-status))

(use-package restclient
  :ensure t
  :mode ("\\.rest\\'" . restclient-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package helm
  :ensure t
  :bind ("C-f" . helm-occur))

(use-package helm-ag :ensure t)
(use-package helm-cider :ensure t)
(use-package helm-org :ensure t)
(use-package helm-org-rifle :ensure t)
(use-package helm-projectile :ensure t)
;; (use-package helm-etags-plus :ensure t)
(use-package undo-tree :ensure t)
(use-package centered-cursor-mode :ensure t)
(use-package async-status :ensure t)
(use-package jvm-mode :ensure t)
(use-package kaocha-runner :ensure t)
(use-package request :ensure t)
(use-package counsel-jq :ensure t)
;; (use-package w3 :ensure t)
(use-package elfeed :ensure t)
;;(use-package edbi :ensure t)
(use-package ag :ensure t)
(use-package treemacs :ensure t)
(use-package olivetti :ensure t)

(use-package org-bullets
  :ensure t
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "INPROGRESS" "PAUSED" "INREVIEW" "BLOCKED" "|" "DONE" "WONTDO")))
  :config
  (setq org-src-tab-acts-natively t)
  :hook
  (org-mode . toggle-word-wrap)
  (org-mode . org-indent-mode)
  (org-maode . (lambda () (org-bullets-mode 1))))

(use-package company
  :ensure t
  :init
  (setq company-global-modes '(not org-mode))
  (setq company-idle-delay 0))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-use-content-types nil)
  (setq cider-stacktrace-default-filters '(project))
  (setq cider-repl-display-help-banner nil)
  (setq cider-enrich-classpath nil)
  :config
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-use-content-types nil)
  (setq cider-stacktrace-default-filters '(project))
  (setq cider-repl-display-help-banner nil)
  :hook
  (cider-repl-mode . company-mode)
  (cider-mode . company-mode)
  (cider-mode . ac-flyspell-workaround))

(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'cider-repl-mode))

(use-package yaml-mode :ensure t)
(use-package toml :ensure t)
(use-package go-mode :ensure t)
(use-package fsharp-mode :ensure t)
(use-package bazel :ensure t)
(use-package fish-mode :ensure t)
(use-package dune-format :ensure t)
(use-package merlin :ensure t)
(use-package merlin-eldoc :ensure t)
(use-package ocamlformat :ensure t)
(use-package tuareg :ensure t)
(use-package utop :ensure t)
(use-package markdown-mode :ensure t)
(use-package lean-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package ediprolog :ensure t)
(use-package flycheck-ocaml :ensure t)
(use-package lsp-treemacs :ensure t)
(use-package lsp-ui :ensure t)
(use-package yasnippet :ensure t)

;;
;; Language support
;;
(use-package lsp-mode
  :ensure t
  :config
  (load "atl-mode.el")
  (setq atl-lsp-jar "/Users/dmatysiak/apps/atl-lsp.jar")
  ;;(setq atl-lsp-args '("--project-file" "/Users/dmatysiak/repos/2-atl/atl-lsp/dev-resources/test/interactions/find-project-file-fails/other.edn"))
  (add-to-list 'auto-mode-alist '("\\.atl\\'" . atl-mode)))

(use-package antlr-mode
  :ensure t
  :mode ("\\.g4\\'" . antlr-mode))

(use-package paredit :ensure t)
(use-package rainbow-blocks :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package rainbow-identifiers :ensure t)

(defun turn-on-paredit () (paredit-mode t))

(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

(use-package sayid :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (sayid-setup-package)
  :hook
  (clojure-mode . turn-on-paredit)
  (clojure-mode . lsp)
  (clojurescript-mode . lsp)
  (clojurec-mode . lsp))

(use-package adoc-mode
  :ensure t
  :mode ("\\.adoc\\'" . adoc-mode))

;; (use-package highlight-sexp
;;   :ensure t
;;   :hook
;;   (clojure-mode . highlight-sexp-mode)
;;   (lisp-mode . highlight-sexp-mode)
;;   (emacs-lisp-mode . highlight-sexp-mode))


(use-package dockerfile-mode :ensure t)
(use-package vlf :ensure t)
;;(use-package vlf-setup :ensure t)
(use-package jira-markup-mode
  :ensure t
  :mode ("\\.jira\\'" . jira-markup-mode))

;;
;; Themes
;;
(use-package haki-theme
  :ensure t
  :custom-face
  (haki-region ((t (:background "#2e8b57" :foreground "#ffffff"))))
  (haki-highlight ((t (:background "#fafad2" :foreground "#000000"))))
  :config
  (load-theme 'haki t)
  (set-frame-font "Iosevka Term Slab 14" nil t)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#6c7b8b"
                      :box '(:line-width (5 . 1) :color "dark cyan" :style released-button)
                      :weight 'medium
                      :height 0.9))

(use-package tron-legacy-theme :ensure t)
(use-package plan9-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package night-owl-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package birds-of-paradise-plus-theme :ensure t)
(use-package blackboard-theme :ensure t)
(use-package color-theme-sanityinc-solarized :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)
(use-package overcast-theme :ensure t)
(use-package green-is-the-new-black-theme :ensure t)
(use-package green-screen-theme :ensure t)
;;(use-package eziam-theme :ensure t)
(use-package pulsing-cursor
  :config (pulsing-cursor-mode +1))


;;
;; Games
;;
(use-package gnugo :ensure t)
(use-package chess :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pulsing-cursor chess gnugo vlf-setup emacs-lisp-mode rainbow-identifiers rainbow-delimiters rainbow-blocks org-bullets org-mode yasnippet lsp-treemacs flycheck-ocaml ediprolog csv-mode lean-mode markdown-mode utop tuareg ocamlformat merlin-eldoc merlin dune-format fish-mode bazel fsharp-mode go-mode toml yaml-mode auto-complete ac-cider company olivetti treemacs ag edbi elfeed w3 counsel-jq vlf request kaocha-runner jvm-mode async-status centered-cursor-mode undo-tree helm-etags-plus helm-projectile helm-org-rifle helm-org helm-cider helm-ag helm projectile sayid restclient paredit magit jira-markup-mode haki-theme f adoc-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-inactive ((t (:foreground "#6c7b8b" :box (:line-width (5 . 1) :color "dark cyan" :style released-button) :weight medium :height 0.9)))))
