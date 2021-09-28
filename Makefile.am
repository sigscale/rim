## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = src include ebin doc priv test

.PHONY: release
release: $(PACKAGE_VERSION).tar.gz

$(PACKAGE_VERSION).rel:	$(PACKAGE_NAME).rel
	cp $(PACKAGE_NAME).rel $(PACKAGE_VERSION).rel

$(PACKAGE_VERSION).tar.gz: $(PACKAGE_VERSION).rel sys.config
	$(ERL) -noinput \
		-eval "systools:make_script(\"$(PACKAGE_VERSION)\")" \
		-eval "systools:make_tar(\"$(PACKAGE_VERSION)\", [{dirs, [include,doc]}])" \
		-s init stop

install-data-hook: $(PACKAGE_VERSION).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)

EXTRA_DIST = $(PACKAGE_VERSION).tar.gz

CLEANFILES = $(PACKAGE_VERSION).rel $(PACKAGE_VERSION).script $(PACKAGE_VERSION).boot $(PACKAGE_VERSION).tar.gz

