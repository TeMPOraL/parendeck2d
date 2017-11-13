;;;; Utilities for loading and resolving asset files.

(in-package #:parendeck2d)

(defparameter *asset-load-paths* '() "All absolute path to directories to search when resolving assets.")

(defun resolve-asset-path (path &key directory (search-paths *asset-load-paths*))
  "Turn a relative `PATH' into an absolute pathname. `PATH' can be a pathname or a string. Returns `NIL' if `PATH' does not
resolve to an asset.

Set `DIRECTORY' to T in order to resolve only directory names. Can be useful to resolve them once, and then
use this function again with `SEARCH-PATHS' rebound to a list with the previously returned value to resolve assets in a specific directory."
  (loop for base-path in search-paths
     for full-path = (merge-pathnames path base-path)
     do
       (let ((p (uiop/filesystem:probe-file* full-path :truename t)))
         (when (if directory
                   (uiop/filesystem:directory-exists-p p)
                   (uiop/filesystem:file-exists-p p))
           (return-from resolve-asset-path p)))))
