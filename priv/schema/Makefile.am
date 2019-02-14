## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

schema_DATA = $(abs_builddir)/configData.xsd \
		$(abs_builddir)/genericNrm.xsd \
		$(abs_builddir)/genericRanNrm.xsd \
		$(abs_builddir)/geranNrm.xsd \
		$(abs_builddir)/utranNrm.xsd \
		$(abs_builddir)/eutranNrm.xsd \
		$(abs_builddir)/hnsNrm.xsd \
		$(abs_builddir)/hensNrm.xsd \
		$(abs_builddir)/nrNrm.xsd \
		$(abs_builddir)/ngcNrm.xsd \
		$(abs_builddir)/sliceNrm.xsd \
		$(abs_builddir)/epcNrm.xsd \
		$(abs_builddir)/coreNrm.xsd \
		$(abs_builddir)/imsNrm.xsd \
		$(abs_builddir)/stnNrm.xsd \
		$(abs_builddir)/sumNrm.xsd \
		$(abs_builddir)/repeaterNrm.xsd \
		$(abs_builddir)/transportNrm.xsd \
		$(abs_builddir)/sonPolicyNrm.xsd \
		$(abs_builddir)/stateManagementIRP.xsd \
		$(abs_builddir)/inventoryNrm.xsd \
		$(abs_builddir)/inventoryNrmAlt2.xsd \
		$(abs_builddir)/geranNrm.json \
		$(abs_builddir)/utranNrm.json \
		$(abs_builddir)/eutranNrm.json \
		$(abs_builddir)/sonPolicyNrm.json \
		$(abs_builddir)/ResourceFunctionSpecification.json \
		$(abs_builddir)/BssSpecification.json \
		$(abs_builddir)/resourceCatalogManagement.json \
		$(abs_builddir)/resourceInventoryManagement.json

$(abs_builddir)/%.xsd:	$(srcdir)/%.xsd
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
		for i in `ls *.xsd`; do \
			if xmllint --noout $$i; then \
				echo "schema $$i is valid"; \
			else \
				echo "schema $$i is invalid"; \
				exit 1; \
			fi; \
		done; \
	else \
		echo "xmllint needed to validate XML Schema"; \
	fi
	@if ajv help > /dev/null 2>&1; then \
		ajv compile -s genericNrm.json; \
		ajv compile -s geranNrm.json; \
		ajv compile -s utranNrm.json; \
		ajv compile -s eutranNrm.json; \
		ajv compile -s sonPolicyNrm.json; \
		ajv compile -s resourceCatalogManagement.json; \
		ajv compile -s ResourceFunctionSpecification.json \
			-r resourceCatalogManagement.json; \
		ajv compile -s BssSpecification.json \
			-r ResourceFunctionSpecification.json \
			-r resourceCatalogManagement.json; \
		ajv compile -s resourceInventoryManagement.json; \
	else \
		echo "ajv-cli needed to validate JSON Schema"; \
	fi

