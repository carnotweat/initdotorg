(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/"
         :html-extension "html"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :publishing-function (org-html-publish-to-html)
         :html-preamble nil
	 :html-postamble nil
         :html-head-extra
         "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://sdf.org/~sameers/my-blog.xml\"
                title=\"RSS feed for sdf.org\">")))
(add-to-list 'org-publish-project-alist
             '("blog-rss"
		:base-directory "~/"
		:base-extension "org"
		:publishing-directory "~/public_html/"
		:publishing-function (org-rss-publish-to-rss)
		:html-link-home "http://sdf.org/~sameers"
		:html-link-use-abs-url t
		:exclude ".*"
		:include ("my-blog.org")))
(add-to-list 'org-publish-project-alist
             '("misc"
		:base-directory "/home/s/org"
		:html-extension "html"
		:base-extension "org"
		:publishing-directory "~/org_html/"
		:publishing-function (org-html-publish-to-html)
		:html-link-home "http://sdf.org/~sameers"
		))
(add-to-list 'org-publish-project-alist
             '("misc-rss"
		:base-directory "/home/s/org"
		:base-extension "org"
		:publishing-directory "~/org_html/"
		:publishing-function (org-rss-publish-to-rss)
		:html-link-home "http://sdf.org/~sameers"
		:html-link-use-abs-url t
)) 
