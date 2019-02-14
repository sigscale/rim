## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

schema_DATA = $(abs_builddir)/ResourceFunctionSpecification.json \
		$(abs_builddir)/BssSpecification.json \
		$(abs_builddir)/resourceCatalogManagement.json

$(abs_builddir)/%.json:	$(srcdir)/%.json
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

check_DATA = .schema_ok

.schema_ok:
	@if ajv help > /dev/null 2>&1; then \
		ajv compile -s ResourceFunctionSpecification.json \
			-r resourceCatalogManagement.json; \
		ajv compile -s BssSpecification.json \
			-r ResourceFunctionSpecification.json \
			-r resourceCatalogManagement.json; \
	fi

