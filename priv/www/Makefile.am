## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

www_DATA = $(abs_builddir)/build/polymer.json

LINKS = index.html manifest.json images src

MODULES = inventory-management.js \
		inventory-list.js \
		catalog-list.js \
		category-list.js \
		user-list.js \
		style-element.js

$(abs_builddir)/index.html:
	for i in $(LINKS); do \
		if [ ! -e $(abs_builddir)/$$i ]; then \
			$(LN_S) $(srcdir)/$$i; \
		fi; \
	done
	if [ ! -e $(abs_builddir)/polymer.json ]; then \
		cp $(srcdir)/polymer.json $(abs_builddir); \
	fi

package.json: yarn.lock

yarn.lock:
	yarn add d3
	yarn install

$(abs_builddir)/build/polymer.json: package.json $(abs_builddir)/index.html ${patsubst %,$(abs_builddir)/src/%,$(MODULES)}
	yarn run build:static

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)
	cp -R $(abs_builddir)/build/esm-bundled/* $(DESTDIR)$(wwwdir)

clean-local:
	-rm -rf yarn.lock build
	for i in $(LINKS); do \
		if [ -L $$i ]; then \
			rm $$i; \
		fi; \
	done
	
