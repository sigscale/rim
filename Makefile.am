## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = src include ebin doc priv test

script_SCRIPTS = scripts/install_tables.escript \
		scripts/join_cluster.escript

RELEASE_NAME = $(PACKAGE_NAME)-$(PACKAGE_VERSION)

.PHONY: release
release: $(RELEASE_NAME).tar.gz

$(RELEASE_NAME).rel: $(PACKAGE_NAME).rel
	cp $< $@

ERLTMP := $(shell mktemp -d)

$(RELEASE_NAME).tar.gz: $(RELEASE_NAME).rel sys.config
	mkdir -p $(ERLTMP)/lib/$(RELEASE_NAME)
	cp -r $(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR)/$(RELEASE_NAME)/* $(ERLTMP)/lib/$(RELEASE_NAME)
	ERL_LIBS=$(ERLTMP)/lib:$(ERL_LIBS) RELDIR=releases $(ERL) -noinput -boot_var OTPHOME lib \
			-eval "systools:make_script(\"$(RELEASE_NAME)\", [{variables, [{\"OTPHOME\", \"$(ERLTMP)\"}]}])" \
			-eval "systools:make_tar(\"$(RELEASE_NAME)\", [{dirs, [include,mibs,doc]}])" \
			-s init stop
	rm -rf $(ERLTMP)

install-data-hook: $(RELEASE_NAME).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/db
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/tls
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/http

EXTRA_DIST = $(RELEASE_NAME).tar.gz

CLEANFILES = $(RELEASE_NAME).rel $(RELEASE_NAME).script $(RELEASE_NAME).boot $(RELEASE_NAME).tar.gz

