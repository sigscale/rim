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
		$(abs_builddir)/NNRncHandOver.xsd \
		$(abs_builddir)/genericNrm.json \
		$(abs_builddir)/geranNrm.json \
		$(abs_builddir)/utranNrm.json \
		$(abs_builddir)/eutranNrm.json \
		$(abs_builddir)/nrNrm.json \
		$(abs_builddir)/ngcNrm.json \
		$(abs_builddir)/sonPolicyNrm.json \
		$(abs_builddir)/stateManagementIRP.json \
		$(abs_builddir)/sliceNrm.json \
		$(abs_builddir)/epcNrm.json \
		$(abs_builddir)/coreNrm.json \
		$(abs_builddir)/imsNrm.json \
		$(abs_builddir)/ResourceFunctionSpec.json \
		$(abs_builddir)/BssFunctionSpec.json \
		$(abs_builddir)/BtsSiteMgrSpec.json \
		$(abs_builddir)/GsmCellSpec.json \
		$(abs_builddir)/RncFunctionSpec.json \
		$(abs_builddir)/NodeBFunctionSpec.json \
		$(abs_builddir)/UtranGenericCellSpec.json \
		$(abs_builddir)/UtranCellFDDSpec.json \
		$(abs_builddir)/UtranCellTDDSpec.json \
		$(abs_builddir)/UtranCellTDDLcrSpec.json \
		$(abs_builddir)/UtranCellTDDHcrSpec.json \
		$(abs_builddir)/ENBFunctionSpec.json \
		$(abs_builddir)/EUtranGenericCellSpec.json \
		$(abs_builddir)/EUtranCellFDDSpec.json \
		$(abs_builddir)/EUtranCellTDDSpec.json \
		$(abs_builddir)/ServingGWFunctionSpec.json \
		$(abs_builddir)/PGWFunctionSpec.json \
		$(abs_builddir)/EPDGFunctionSpec.json\
		$(abs_builddir)/MMEFunctionSpec.json \
		$(abs_builddir)/PCRFFunctionSpec.json \
		$(abs_builddir)/MscServerFunctionSpec.json \
		$(abs_builddir)/CsMgwFunctionSpec.json \
		$(abs_builddir)/SgsnFunctionSpec.json \
		$(abs_builddir)/GgsnFunctionSpec.json \
		$(abs_builddir)/ASFunctionSpec.json \
		$(abs_builddir)/HSSFunctionSpec.json \
		$(abs_builddir)/PCSCFFunctionSpec.json \
		$(abs_builddir)/SCSCFFunctionSpec.json \
		$(abs_builddir)/ICSCFFunctionSpec.json \
		$(abs_builddir)/GNBDUFunctionSpec.json \
		$(abs_builddir)/GNBCUCPFunctionSpec.json \
		$(abs_builddir)/GNBCUUPFunctionSpec.json \
		$(abs_builddir)/NRCellDUSpec.json \
		$(abs_builddir)/NRCellCUSpec.json \
		$(abs_builddir)/NRSectorCarrierSpec.json \
		$(abs_builddir)/NetworkSliceSpec.json \
		$(abs_builddir)/NetworkSliceSubnetSpec.json \
		$(abs_builddir)/AMFFunctionSpec.json \
		$(abs_builddir)/SMFFunctionSpec.json \
		$(abs_builddir)/UPFFunctionSpec.json \
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
		if ! ajv compile -s ngcNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s sonPolicyNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s stateManagementIRP.json; \
			then exit 1; fi; \
		if ! ajv compile -s sliceNrm.json; \
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
		if ! ajv compile -s BssFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BtsSiteMgrSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GsmCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RncFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NodeBFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellFDDSpec.json \
			-r UtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDSpec.json \
			-r UtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDLcrSpec.json \
			-r UtranCellTDDSpec.json \
			-r UtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDHcrSpec.json \
			-r UtranCellTDDSpec.json \
			-r UtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ENBFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellFDDSpec.json \
			-r EUtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellTDDSpec.json \
			-r EUtranGenericCellSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ServingGWFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PGWFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EPDGFunctionSpec.json\
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MMEFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCRFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MscServerFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CsMgwFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgsnFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GgsnFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ASFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s HSSFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCSCFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SCSCFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCSCFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBDUFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUCPFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUUPFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellDUSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellCUSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRSectorCarrierSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSliceSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSliceSubnetSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AMFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SMFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UPFFunctionSpec.json \
			-r ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s resourceInventoryManagement.json; \
			then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON Schema"; \
	fi

