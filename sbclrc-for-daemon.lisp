(push :debug *features*)
(require :sb-posix)
(require :asdf)
(push (pathname (format nil "~A/tools/slime/"
			(sb-posix:getenv "PWD")))
      asdf:*central-registry*)
