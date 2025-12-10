(use-modules
 (gnu packages python)
 (gnu packages python-build)
 (gnu packages python-science)
 (gnu packages python-xyz))

(packages->manifest
 (list python
       python-black
       python-lsp-server
       python-lsp-black
       python-shapely
       python-scipy))
