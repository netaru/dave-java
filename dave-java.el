;;; dave-java.el --- Some extra configuration for java  -*- lexical-binding: t -*-

;; Author:    David Jonsson <david.jonsson306@gmail.com>
;; URL:       N/A
;; Version:   0.0.1
;; Package-Requires: ((emacs "26.1") (consult "0.7.0") (f "0.6.0"))

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

;; Some custom variables to ease the configuration of lsp-java. Convenience
;; function to download lombok etc.
;; 
;; Improvements:
;;  - Include downloading of a formatter

;;; Code:
(defcustom dave-java-enable-lombok t
  "If lombok should be used in lsp-java"
  :group 'dave-java
  :type 'boolean)

(defcustom dave-java-lombok-path (concat (getenv "HOME") "/.local/share/dave/libs/lombok.jar")
  "Path to lombok jar file"
  :group 'dave-java
  :type 'file)

(defcustom dave-java-lombok-download-url "https://repo1.maven.org/maven2/org/projectlombok/lombok/1.18.38/lombok-1.18.38.jar"
  "Url to lombok file"
  :group 'dave-java
  :type 'string)

(defcustom dave-java-java-home "/usr/lib/jvm/java-21-openjdk"
  "Path to java home for the jvm running jdtls."
  :group 'dave-java
  :type 'directory)

(defcustom dave-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java"
  "Path of the java executable. Used for lsp and other development tools."
  :group 'dave-java
  :type '(file :must-match t))

(defcustom dave-java-format-program-path (concat (getenv "HOME") "/.local/bin/palantirw")
  "Path of the format executable. Used for formatting java code."
  :group 'dave-java
  :type 'file)

(defcustom dave-java-format-program-args '("--skip-reflowing-long-strings" "--palantir" "-")
  "Arguments passed to the format program `dave-java-format-program-path'."
  :group 'dave-java
  :type 'list)

(defun dave-java-mvn-read-artifact-ids ()
  "Read the project names from 'pom.xml' files.

Requires 'find', 'xargs' and 'xq' to be installed and present on the exec-path.
Macos requires 'yq' symlinked to 'xq'.
"
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (candidates (split-string
                      (shell-command-to-string "find . -name 'pom.xml' -type f -maxdepth 3 -print0 | xargs -0 -I{} xq -r .project.artifactId {}")
                      "\n"
                      t)))
    candidates))

(defun dave-java-mvn-run-test-unit (goal &optional args)
  "Run test unit with maven. Has to be in a `java-ts-mode' file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (root (locate-dominating-file default-directory "pom.xml"))
         (default-directory root)
         (mvn-cmd (format "cd %s && mvn -Dtest=%s %s %s" root class-name (if args "-Dmaven.surefire.debug" "") goal)))
    (compilation-start mvn-cmd t (lambda (&rest _) (generate-new-buffer-name (format "*%s %s*" root class-name))))))

(defun dave-java-mvn-run-test-case (goal &optional args)
  "Run test case with maven. `consult-imenu--items' is Used to choose the target function to test. Has to be in a `java-ts-mode' file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (class-name (car (split-string
                           (car (last (split-string file-name "/")))
                           "\\.")))
         (test-case (car (imenu-choose-buffer-index "Test case: " (consult-imenu--items))))
         (pos `(,(+ 1 (string-search "." test-case)) . ,(string-search "(" test-case)))
         (test-name (substring test-case (car pos) (cdr pos)))
         (root (locate-dominating-file default-directory "pom.xml"))
         (default-directory root)
         (mvn-cmd (format "cd %s && mvn -Dtest=%s#%s %s %s" root class-name test-name (if args "-Dmaven.surefire.debug" "") goal)))
    (compilation-start mvn-cmd t (lambda (&rest _) (generate-new-buffer-name (format "*%s %s %s*" root class-name test-name))))))

;;;###autoload
(defun dave-java-mvn-run-test-unit-rebuild (&optional debug)
  "Run mvn test on the current test buffer. By rebuilding with maven."
  (interactive "P")
  (dave-java-mvn-run-test-unit "test" debug))

;;;###autoload
(defun dave-java-mvn-run-test-unit-surefire (&optional debug)
  "Run mvn surefire:test on the current test buffer. By using the built files in the target map."
  (interactive "P")
  (dave-java-mvn-run-test-unit "surefire:test" debug))

;;;###autoload
(defun dave-java-mvn-run-test-case-rebuild (&optional debug)
  "Run mvn test on the current test buffer, choose a function to test. By rebuilding with maven."
  (interactive "P")
  (dave-java-mvn-run-test-case "test" debug))

;;;###autoload
(defun dave-java-mvn-run-test-case-surefire (&optional debug)
  "Run mvn surefire:test on the current test buffer, choose a function to test. By using the built files in the target map."
  (interactive "P")
  (dave-java-mvn-run-test-case "surefire:test" debug))

;;;###autoload
(defun dave-java-palantir-format-buffer ()
  (interactive)
  (if (or (file-exists-p dave-java-format-program-path)
          (executable-find dave-java-format-program-path))
      (let ((buffer (generate-new-buffer "*dave-java-format-buffer-temp*")))
        (apply #'call-process-region nil nil dave-java-format-program-path nil `(,buffer, nil) nil dave-java-format-program-args)
        (replace-buffer-contents buffer)
        (kill-buffer buffer))))

;;;###autoload
(defun dave-java-initialise-class (filename)
  (interactive)
  (let* ((id "src/\\(main\\|test\\)/java")
         (where (string-match id filename))
         (suffix (substring filename (+ 1 (match-end 0)))))
    `(,(substring filename 0 (- (string-width suffix))) . ,(replace-regexp-in-string (regexp-quote "/") "." (file-name-directory suffix) nil 'literal))))

;;;###autoload
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

;;;###autoload
(defun dave-java-install-lombok ()
  (interactive)
  (unless (file-exists-p dave-java-lombok-path)
    (progn
      (make-directory (file-name-directory dave-java-lombok-path) t)
      (url-copy-file dave-java-lombok-download-url dave-java-lombok-path))))


(provide 'dave-java)
;;; dave-java.el ends here
