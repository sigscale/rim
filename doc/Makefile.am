## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = im_app.html \
			im_sup.html \
			im.html \
			im_specification.html \
			im_xml_cm_bulk.html \
			im_xml_geran.html \
			im_xml_eutran.html \
			im_xml_epcn3ai.html \
			im_xml_nr.html \
			im_xml_5gc.html \
			im_xml_slice.html \
			im_server.html \
			mod_im_rest_accepted_content.html \
			mod_im_rest_get.html \
			mod_oauth.html \
			im_rest.html \
			im_rest_res_user.html \
			im_rest_res_catalog.html \
			im_rest_res_category.html \
			im_rest_res_candidate.html \
			im_rest_res_specification.html \
			im_rest_res_resource.html \
			im_rest_res_http.html \
			im_rest_query_scanner.html \
			im_rest_query_parser.html \
			im_rest_pagination_sup.html \
			im_rest_pagination_server.html \
			im_rest_res_role.html \
			im_rest_hub_sup.html \
			im_rest_hub_fsm.html \
			im_event.html \
			im_rest_res_hub_role.html \
			im_statistics_sup.html \
			im_statistics_server.html \
			im_rest_res_health.html \
			im_rest_res_prometheus.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css \
		sigscale-logo.png supervision.png

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/supervision.png .

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

