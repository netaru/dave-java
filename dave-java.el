;;; dave-java.el --- Some extra configuration for java  -*- lexical-binding: t -*-

;; Author:    David Jonsson <david.jonsson306@gmail.com>
;; URL:       N/A
;; Version:   0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fill here

;;; Code:
(defcustom dave-java-enable-lombok t
  "If lombok should be used in lsp-java"
  :group 'dave-java
  :type 'boolean)

(defcustom dave-java-lombok-path (concat (getenv "HOME") "/.local/share/dave/libs/lombok.jar")
  "Path to lombok jar file"
  :group 'dave-java
  :type 'string)

(defcustom dave-java-lombok-download-url "https://repo1.maven.org/maven2/org/projectlombok/lombok/1.18.38/lombok-1.18.38.jar"
  "url to lombok file"
  :group 'dave-java
  :type 'string)

(defcustom dave-java-java-home "/usr/lib/jvm/default"
  "docstring"
  :group 'dave-java
  :type 'string)

(defcustom dave-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java"
  "Path of the java executable. Used for lsp and other development tools."
  :group 'dave-java
  :type 'string)

;; A function to format stdin xml and put the result in the same buffer
(defun dave-java-palantir-format-buffer ()
  (interactive)
  (let((buffer (generate-new-buffer "*java-palantir-format-buffer-temp*")))
    (call-process-region nil nil (concat (getenv "HOME") "/.local/bin/palantirw") nil `(,buffer, nil) nil "--skip-reflowing-long-strings" "--palantir" "-")
    (replace-buffer-contents buffer)
    (kill-buffer buffer)))

(defun dave-java-initialise-class (filename)
  (interactive)
  (let* ((id "src/\\(main\\|test\\)/java")
         (where (string-match id filename))
         (suffix (substring filename (+ 1 (match-end 0)))))
    (progn
      `(,(substring filename 0 (- (string-width suffix))) . ,(replace-regexp-in-string (regexp-quote "/") "." (file-name-directory suffix) nil 'literal)))))

(defun dave-java-create-class ()
  (interactive)
  (let* ((class-data (dave-java-initialise-class buffer-file-name))
         (class-path (read-string "class name: " (cdr class-data)))
         (package-index (string-match "\\.[^\\.]*?$" class-path))
         (class-name (substring class-path (+ 1 package-index)))
         (package-name (substring class-path 0 (- (- 1) (string-width class-name))))
         (filename (concat (car class-data) (replace-regexp-in-string (regexp-quote ".") "/" class-path) ".java"))
         (directory (file-name-directory filename)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (unless (file-exists-p filename)
      (f-write-text (concat "package " package-name ";\n\npublic class " class-name " {\n\n}\n") 'utf-8 filename))
    (find-file filename)))

(defun dave-java-install-lombok ()
  (interactive)
  (unless (file-exists-p dave-java-lombok-path)
    (progn
      (make-directory (file-name-directory dave-java-lombok-path) t)
      (url-copy-file dave-java-lombok-download-url dave-java-lombok-path))))

(use-package lsp-sonarlint-java
  :ensure nil
  :custom
  (lsp-sonarlint-java-enabled t))

(use-package lsp-java
  :general
  (general-nmap
    :keymaps 'java-ts-mode-map
    ",fn" 'dave-java-create-class
    ",tb" 'lsp-jt-browser
    ",tl" 'lsp-jt-lens-mode
    ",tr" 'lsp-jt-report-open
    ",pb" 'lsp-java-build-project
    "=" 'dave-java-palantir-format-buffer)
  (general-nmap
    :keymaps 'lsp-jt-mode-map
    "r" 'lsp-jt-run
    "d" 'lsp-jt-debug)
  :custom
  (lsp-java-java-path dave-java-java-path)
  (lsp-java-autobuild-enabled nil)
  :config
  (setenv "JAVA_HOME" dave-java-java-home)
  (if (and dave-java-enable-lombok
           (file-exists-p dave-java-lombok-path))
      (add-to-list 'lsp-java-vmargs (concat "-javaagent:" dave-java-lombok-path) t)))

(use-package dap-java
  :ensure nil)

;;;###autoload

(provide 'dave-projectile-utils)
;;; dave-java.el ends here
