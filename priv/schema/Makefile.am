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
		$(abs_builddir)/genericNrm.json \
		$(abs_builddir)/geranNrm.json \
		$(abs_builddir)/utranNrm.json \
		$(abs_builddir)/eutranNrm.json \
		$(abs_builddir)/nrNrm.json \
		$(abs_builddir)/sonPolicyNrm.json \
		$(abs_builddir)/stateManagementIRP.json \
		$(abs_builddir)/epcNrm.json \
		$(abs_builddir)/coreNrm.json \
		$(abs_builddir)/imsNrm.json \
		$(abs_builddir)/ResourceFunctionSpec.json \
		$(abs_builddir)/BssFunctionSpecification.json \
		$(abs_builddir)/BtsSiteMgrSpecification.json \
		$(abs_builddir)/GsmCellSpecification.json \
		$(abs_builddir)/RncFunctionSpecification.json \
		$(abs_builddir)/NodeBFunctionSpecification.json \
		$(abs_builddir)/UtranGenericCellSpecification.json \
		$(abs_builddir)/UtranCellFDDSpecification.json \
		$(abs_builddir)/UtranCellTDDSpecification.json \
		$(abs_builddir)/UtranCellTDDLcrSpecification.json \
		$(abs_builddir)/UtranCellTDDHcrSpecification.json \
		$(abs_builddir)/ENBFunctionSpecification.json \
		$(abs_builddir)/EUtranGenericCellSpecification.json \
		$(abs_builddir)/EUtranCellFDDSpecification.json \
		$(abs_builddir)/EUtranCellTDDSpecification.json \
		$(abs_builddir)/ServingGWFunctionSpecification.json \
		$(abs_builddir)/PGWFunctionSpecification.json \
		$(abs_builddir)/MMEFunctionSpecification.json \
		$(abs_builddir)/PCRFFunctionSpecification.json \
		$(abs_builddir)/MscServerFunctionSpecification.json \
		$(abs_builddir)/CsMgwFunctionSpecification.json \
		$(abs_builddir)/SgsnFunctionSpecification.json \
		$(abs_builddir)/GgsnFunctionSpecification.json \
		$(abs_builddir)/ASFunctionSpecification.json \
		$(abs_builddir)/HSSFunctionSpecification.json \
		$(abs_builddir)/PCSCFFunctionSpecification.json \
		$(abs_builddir)/SCSCFFunctionSpecification.json \
		$(abs_builddir)/ICSCFFunctionSpecification.json \
		$(abs_builddir)/GNBDUFunctionSpecification.json \
		$(abs_builddir)/GNBCUCPFunctionSpecification.json \
		$(abs_builddir)/GNBCUUPFunctionSpecification.json \
		$(abs_builddir)/NRCellDUSpecification.json \
		$(abs_builddir)/NRCellCUSpecification.json \
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
		if ! ajv compile -s genericNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s geranNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s utranNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s eutranNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s nrNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s sonPolicyNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s stateManagementIRP.json; \
			then exit 1; fi; \
		if ! ajv compile -s epcNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s coreNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s imsNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BssFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BtsSiteMgrSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GsmCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RncFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NodeBFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellFDDSpecification.json \
			-r UtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDSpecification.json \
			-r UtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDLcrSpecification.json \
			-r UtranCellTDDSpecification.json \
			-r UtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDHcrSpecification.json \
			-r UtranCellTDDSpecification.json \
			-r UtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ENBFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellFDDSpecification.json \
			-r EUtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellTDDSpecification.json \
			-r EUtranGenericCellSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ServingGWFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PGWFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MMEFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCRFFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MscServerFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CsMgwFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgsnFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GgsnFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ASFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s HSSFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCSCFFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SCSCFFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCSCFFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBDUFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUCPFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUUPFunctionSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellDUSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellCUSpecification.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s resourceInventoryManagement.json; \
			then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON Schema"; \
	fi

