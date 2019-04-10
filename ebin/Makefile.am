## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = sigscale_im.app
EXTRA_DIST = sigscale_im.app

beam_DATA = im_app.beam \
			im_sup.beam \
			im.beam \
			im_specification.beam \
			im_xml_cm_bulk.beam \
			im_xml_generic.beam \
			im_xml_geran.beam \
			im_xml_utran.beam \
			im_xml_epc.beam \
			im_xml_core.beam \
			im_xml_zte.beam \
			im_server.beam \
			mod_im_rest_accepted_content.beam \
			mod_im_rest_get.beam \
			mod_im_rest_post.beam \
			im_rest.beam \
			im_rest_res_user.beam \
			im_rest_res_catalog.beam \
			im_rest_res_category.beam \
			im_rest_res_candidate.beam \
			im_rest_res_specification.beam \
			im_rest_res_resource.beam \
			im_rest_query_scanner.beam \
			im_rest_query_parser.beam \
			im_rest_pagination_sup.beam \
			im_rest_pagination_server.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/im.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include $<

im_rest_query_scanner.beam:$(top_builddir)/src/im_rest_query_scanner.erl
	$(ERLC) $<

im_rest_query_parser.beam:$(top_builddir)/src/im_rest_query_parser.erl
	$(ERLC) $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

