## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

www_DATA = $(abs_builddir)/build/polymer.json

LINKS = index.html manifest.json images src

MODULES = inventory-management.js \
		inventory-list.js \
		inventory-topology.js \
		catalog-list.js \
		catalog-add.js \
		catalog-update.js \
		category-list.js \
		category-add.js \
		category-update.js \
		candidate-list.js \
		candidate-add.js \
		candidate-update.js \
		specification-list.js \
		specification-add.js \
		specification-update.js \
		specification-topology.js \
		inventory-help.js \
		inventory-add.js \
		rule-list.js \
		rule-update.js \
		http-list.js \
		user-list.js \
		style-element.js

$(abs_builddir)/index.html:
	for i in $(LINKS); do \
		if [ ! -e $(abs_builddir)/$$i ]; then \
			$(LN_S) $(srcdir)/$$i; \
		fi; \
	done

$(abs_builddir)/sw-precache-config.js:
	if [ ! -e $(abs_builddir)/sw-precache-config.js ]; then \
		cp $(srcdir)/sw-precache-config.js $(abs_builddir); \
	fi

package.json: yarn.lock

yarn.lock:
	yarn add d3
	yarn install

$(abs_builddir)/build/polymer.json: package.json $(abs_builddir)/index.html $(abs_builddir)/sw-precache-config.js ${patsubst %,$(abs_builddir)/src/%,$(MODULES)}
	if [ ! -e $(abs_builddir)/polymer.json ]; then \
		cp $(srcdir)/polymer.json $(abs_builddir); \
	fi
	polymer build

check_DATA = .polymer_lint_ok

.polymer_lint_ok:
	yarn run lint

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)
	cp -R $(abs_builddir)/build/esm-bundled/* $(DESTDIR)$(wwwdir)

clean-local:
	-rm -rf yarn.lock build
	-rm $(abs_builddir)/src/inventory-help.js
	for i in $(LINKS); do \
		if [ -L $$i ]; then \
			rm $$i; \
		fi; \
	done

$(abs_builddir)/src/inventory-help.js: $(srcdir)/src/inventory-help.js-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%PACKAGE%!$(PACKAGE)!' < $< > $@

