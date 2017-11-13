;;;; Utilities for loading and resolving asset files.

(in-package #:parendeck2d)

(defparameter *asset-search-paths* '() "All absolute path to directories to search when resolving assets.")

(defun add-asset-search-path (directory-path)
  "Register `DIRECTORY-PATH' as an additional path `RESOLVE-ASSET-PATH' will use to search for assets.
`DIRECTORY-PATH' must be an absolute path to a directory."

  (pushnew directory-path *asset-search-paths* :test #'equal))

(defun resolve-asset-path (path &key directory (search-paths *asset-search-paths*))
  "Turn a relative `PATH' into an absolute pathname. `PATH' can be a pathname or a string. Returns `NIL' if `PATH' does not
resolve to an asset.

If `PATH' represents an absolute path, the return value will be an absolute pathname to the specified file or directory, if it exists.

Set `DIRECTORY' to T in order to resolve only directory names. Can be useful to resolve them once, and then
use this function again with `SEARCH-PATHS' rebound to a list with the previously returned value to resolve assets in a specific directory.

Directories in `SEARCH-PATHS' are searched left-to-right. Files or directories found earlier take precedence."
  (loop for base-path in search-paths
     for full-path = (merge-pathnames path base-path)
     do
       (let ((p (uiop/filesystem:probe-file* full-path :truename t)))
         (when (if directory
                   (uiop/filesystem:directory-exists-p p)
                   (uiop/filesystem:file-exists-p p))
           (return-from resolve-asset-path p)))))


(defun install-default-asset-search-paths ()
  "Installs appropriate base paths for searching assets, depending whether we're in a development or production environment."
  (add-asset-search-path (merge-pathnames "assets/" (engine-base-directory)))
  ;; TODO handle production environment (dumped image)
  
  (log:debug "Installed default asset search paths." *asset-search-paths*))
