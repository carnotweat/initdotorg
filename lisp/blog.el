(setq org-publish-project-alist
      '(("orgfiles"
          :base-directory "~/org/"
          :base-extension "org"
	  :recursive t
          :publishing-directory "/ssh:sameers@sdf.org:~/arpa/ns/s/sameers/public_html/notebook/"
          :publishing-function org-html-publish-to-html
          :exclude "PrivatePage.org"   ;; regexp
          :headline-levels 3
          :section-numbers nil
          :with-toc nil
          :html-head "<link rel=\"stylesheet\"
                  href=\"../other/mystyle.css\" type=\"text/css\"/>"
          :html-preamble t)

         ("images"
          :base-directory "~/images/"
          :base-extension "jpg\\|gif\\|png"
	  :recursive t
          :publishing-directory "/ssh:sameers@sdf.org:~/arpa/ns/s/sameers/public_html/media/"
          :publishing-function org-publish-attachment)

         ("other"
          :base-directory "~/other/"
          :base-extension "css\\|el"
	  :recursive t
          :publishing-directory "/ssh:sameers@sdf.org:~/arpa/ns/s/sameers/public_html/css/"
          :publishing-function org-publish-attachment)
         ("website" :components ("orgfiles" "images" "other"))))
