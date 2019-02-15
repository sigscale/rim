## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

examples_DATA = $(abs_builddir)/geran.xml \
		$(abs_builddir)/utran.xml \
		$(abs_builddir)/eutran.xml \
		$(abs_builddir)/core.xml \
		$(abs_builddir)/epc.xml \
		$(abs_builddir)/ims.xml \
		$(abs_builddir)/catalog.json \
		$(abs_builddir)/category.json \
		$(abs_builddir)/candidate.json \
		$(abs_builddir)/specification-bss.json \
		$(abs_builddir)/specification-bts.json \
		$(abs_builddir)/specification-cell.json \
		$(abs_builddir)/specification-rnc.json \
		$(abs_builddir)/resource.json

$(abs_builddir)/%.xml:	$(srcdir)/%.xml
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

$(abs_builddir)/%.json:	$(srcdir)/%.json
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

check_DATA = .validation_ok

.validation_ok:
	@if xmllint --version > /dev/null 2>&1; then \
		for i in `ls *.xml`; do \
			if ! xmllint --noout --schema $(top_builddir)/priv/schema/configData.xsd $$i; \
			then exit 1; fi \
		done; \
	else \
		echo "xmllint needed to validate XML"; \
	fi
	@if ajv help > /dev/null 2>&1; then \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d catalog.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d category.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d candidate.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/BssFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-bss.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/BtsSiteMgrSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-bts.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GsmCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-cell.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/RncFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-rnc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceInventoryManagement.json \
				-d resource.json; then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON"; \
	fi

