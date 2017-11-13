(in-package #:parendeck2d)

;;; All basic stuff related to directories.
(defun user-home-directory ()
  "Get current user's home directory."
  (uiop:ensure-directory-pathname (user-homedir-pathname)))

(defun working-directory ()
  "Get current working directory"
  (uiop/os:getcwd))

(defun temporary-directory ()
  "Get the path to a directory, which one can use for throwaway computation."
  (uiop:temporary-directory))

(defun engine-base-directory ()
  "The toplevel directory of the engine."
  (uiop:ensure-directory-pathname (asdf:system-source-directory :parendeck2d)))
