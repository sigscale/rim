## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

CERT_DIR = im_rest_SUITE_data

testmods = im_api_SUITE \
		im_rest_SUITE \
		im_test_lib

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/im.hrl
	$(ERLC) -pa $(top_builddir)/ebin $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d $(CERT_DIR) ]; then mkdir $(CERT_DIR); fi
	@if [ ! -d im_rest_SUITE_data ]; then mkdir im_rest_SUITE_data; fi
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	(cd $(CERT_DIR) && \
		if [ ! -f CAkey.pem -o ! -f CAcert.pem ]; then \
			openssl req -newkey rsa:2048 -nodes -x509 -days 30 \
					-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
					-keyout CAkey.pem -out CAcert.pem; \
		fi; \
		if [ ! -f key.pem -o ! -f cert.csr ]; then \
		openssl req -newkey rsa:2048 -nodes \
				-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
				-keyout key.pem -out cert.csr; \
				chmod 400 key.pem; \
		fi; \
		if [ ! -f extensions ]; then \
			echo "extendedKeyUsage = serverAuth" > extensions; \
			echo "subjectAltName = DNS:$$(hostname)" >> extensions; \
		fi; \
		if [ ! -f cert.pem ]; then \
			openssl x509 -req -CA CAcert.pem -CAkey CAkey.pem -CAcreateserial \
					-extfile extensions -in cert.csr -out cert.pem; \
		fi)
	if [ ! -f im_rest_SUITE_data/key.pem ]; then \
      openssl rsa -in $(CERT_DIR)/key.pem -out im_rest_SUITE_data/key.pem; \
      chmod 400 im_rest_SUITE_data/key.pem; \
   fi; \
   NODENAME=`id -un`
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -noshell -sname $(LOGNAME)-ct -config sys

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e 's#%CERT_DIR%#$(abs_top_builddir)/test/$(CERT_DIR)#' \
			-e 's#%APP_PRIV%#$(abs_top_srcdir)/priv#' \
			-e 's#%SERVER_ROOT%#$(abs_top_builddir)/test#' \
			-e "s#%HTTP_PORT%#$$(($$RANDOM + 32768))#" < $< > $@

clean-local:
	-rm -r *.beam $(CERT_DIR)/CAkey.pem $(CERT_DIR)/key.pem $(CERT_DIR)/extensions $(CERT_DIR)/cert.pem $(CERT_DIR)/cert.csr log/*

