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
		$(abs_builddir)/peeCmonNrm.xsd \
		$(abs_builddir)/epcn3aiNrm.xsd \
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
		$(abs_builddir)/peeCmonNrm.json \
		$(abs_builddir)/epcn3aiNrm.json \
		$(abs_builddir)/inventoryNrm.json \
		$(abs_builddir)/inventoryNrmAlt2.json \
		$(abs_builddir)/ManagedFunction.json \
		$(abs_builddir)/ResourceFunctionSpec.json \
		$(abs_builddir)/BssFunctionSpec.json \
		$(abs_builddir)/BtsSiteMgrSpec.json \
		$(abs_builddir)/GsmCellSpec.json \
		$(abs_builddir)/RncFunctionSpec.json \
		$(abs_builddir)/NodeBFunctionSpec.json \
		$(abs_builddir)/UtranGenericCellSpec.json \
		$(abs_builddir)/UtranGenericCell.json \
		$(abs_builddir)/UtranCellFDDSpec.json \
		$(abs_builddir)/UtranCellTDDSpec.json \
		$(abs_builddir)/UtranCellTDD.json \
		$(abs_builddir)/UtranCellTDDLcrSpec.json \
		$(abs_builddir)/UtranCellTDDHcrSpec.json \
		$(abs_builddir)/IubLinkSpec.json \
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
		$(abs_builddir)/AucFunctionSpec.json \
		$(abs_builddir)/HlrFunctionSpec.json \
		$(abs_builddir)/EirFunctionSpec.json \
		$(abs_builddir)/MnpSrfFunctionSpec.json \
		$(abs_builddir)/CgfFunctionSpec.json \
		$(abs_builddir)/SgwFunctionSpec.json \
		$(abs_builddir)/CbcFunctionSpec.json \
		$(abs_builddir)/IucsLinkSpec.json \
		$(abs_builddir)/IupsLinkSpec.json \
		$(abs_builddir)/IubcLinkSpec.json \
		$(abs_builddir)/ALinkSpec.json \
		$(abs_builddir)/GbLinkSpec.json \
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
		$(abs_builddir)/N3IWFFunctionSpec.json \
		$(abs_builddir)/PCFFunctionSpec.json \
		$(abs_builddir)/AUSFFunctionSpec.json \
		$(abs_builddir)/UDMFunctionSpec.json \
		$(abs_builddir)/UDRFunctionSpec.json \
		$(abs_builddir)/UDSFFunctionSpec.json \
		$(abs_builddir)/NRFFunctionSpec.json \
		$(abs_builddir)/NSSFFunctionSpec.json \
		$(abs_builddir)/SMSFFunctionSpec.json \
		$(abs_builddir)/LMFFunctionSpec.json \
		$(abs_builddir)/NGEIRFunctionSpec.json \
		$(abs_builddir)/SEPPFunctionSpec.json \
		$(abs_builddir)/NWDAFFunctionSpec.json \
		$(abs_builddir)/EP_N2Spec.json \
		$(abs_builddir)/EP_N3Spec.json \
		$(abs_builddir)/EP_N4Spec.json \
		$(abs_builddir)/EP_N6Spec.json \
		$(abs_builddir)/EP_N7Spec.json \
		$(abs_builddir)/EP_N8Spec.json \
		$(abs_builddir)/EP_N9Spec.json \
		$(abs_builddir)/EP_N10Spec.json \
		$(abs_builddir)/EP_N11Spec.json \
		$(abs_builddir)/EP_N12Spec.json \
		$(abs_builddir)/EP_N14Spec.json \
		$(abs_builddir)/EP_N15Spec.json \
		$(abs_builddir)/EP_N16Spec.json \
		$(abs_builddir)/EP_N17Spec.json \
		$(abs_builddir)/EP_N20Spec.json \
		$(abs_builddir)/EP_N22Spec.json \
		$(abs_builddir)/EP_N26Spec.json \
		$(abs_builddir)/EP_NLSSpec.json \
		$(abs_builddir)/EP_NLGSpec.json \
		$(abs_builddir)/EP_SBI_XSpec.json \
		$(abs_builddir)/EP_S5CSpec.json \
		$(abs_builddir)/PEEMonitoredEntitySpec.json \
		$(abs_builddir)/InventoryUnitSpec.json \
		$(abs_builddir)/TmaInventoryUnitSpec.json \
		$(abs_builddir)/AntennaInventoryUnitSpec.json \
		$(abs_builddir)/InventoryUnitNESpec.json \
		$(abs_builddir)/InventoryUnitHwSpec.json \
		$(abs_builddir)/InventoryUnitSwSpec.json \
		$(abs_builddir)/InventoryUnitLicSpec.json \
		$(abs_builddir)/ManagedElementSpec.json \
		$(abs_builddir)/USNFunctionSpec.json \
		$(abs_builddir)/UGWFunctionSpec.json \
		$(abs_builddir)/CGPOMUFunctionSpec.json \
		$(abs_builddir)/iGWBFunctionSpec.json \
		$(abs_builddir)/USCDBFunctionSpec.json \
		$(abs_builddir)/SPSV3FunctionSpec.json \
		$(abs_builddir)/MSCServerIntraOfiSigPointSpec.json \
		$(abs_builddir)/MSCServerOfficeSpec.json \
		$(abs_builddir)/Link_MME_MMESpec.json \
		$(abs_builddir)/Link_HSS_MMESpec.json \
		$(abs_builddir)/Link_MME_SGSNSpec.json \
		$(abs_builddir)/Link_MME_ServingGWSpec.json \
		$(abs_builddir)/Link_ENB_MMESpec.json \
		$(abs_builddir)/EP_RPSpec.json \
		$(abs_builddir)/EP_RP_EPSSpec.json \
		$(abs_builddir)/SubNetworkSpec.json \
		$(abs_builddir)/MobileEdgeHostFunctionSpec.json \
		$(abs_builddir)/DNSRuleSpec.json \
		$(abs_builddir)/LocationServiceSpec.json \
		$(abs_builddir)/MobileEdgeApplicationServiceSpec.json \
		$(abs_builddir)/MobileEdgeApplicationSpec.json \
		$(abs_builddir)/MobileEdgePlatformServiceSpec.json \
		$(abs_builddir)/MobileEdgePlatformSpec.json \
		$(abs_builddir)/RNIServiceSpec.json \
		$(abs_builddir)/TrafficRuleSpec.json \
		$(abs_builddir)/EP_X2CSpec.json \
		$(abs_builddir)/EP_X2USpec.json \
		$(abs_builddir)/EP_NgCSpec.json \
		$(abs_builddir)/EP_NgUSpec.json \
		$(abs_builddir)/EP_XnCSpec.json \
		$(abs_builddir)/EP_XnUSpec.json \
		$(abs_builddir)/EP_F1CSpec.json \
		$(abs_builddir)/EP_F1USpec.json \
		$(abs_builddir)/EP_E1Spec.json \
		$(abs_builddir)/EP_S1USpec.json \
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
		if ! ajv compile -s peeCmonNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s epcn3aiNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s inventoryNrm.json; \
			then exit 1; fi; \
		if ! ajv compile -s inventoryNrmAlt2.json; \
			then exit 1; fi; \
		if ! ajv compile -s resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ResourceFunctionSpec.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BssFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BtsSiteMgrSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GsmCellSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RncFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NodeBFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranGenericCellSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellFDDSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDSpec.json \
			-r UtranGenericCellSpec \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDLcrSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDHcrSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IubLinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ENBFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranGenericCellSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellFDDSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellTDDSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ServingGWFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PGWFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EPDGFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MMEFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCRFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MscServerFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CsMgwFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgsnFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GgsnFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AucFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s HlrFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EirFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MnpSrfFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CgfFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgwFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CbcFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IucsLinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IupsLinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IubcLinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ALinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GbLinkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ASFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s HSSFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCSCFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SCSCFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ICSCFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBDUFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUCPFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GNBCUUPFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellDUSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRCellCUSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRSectorCarrierSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSliceSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSliceSubnetSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AMFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SMFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UPFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s N3IWFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AUSFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDMFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDRFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDSFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NSSFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SMSFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s LMFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NGEIRFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SEPPFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NWDAFFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N2Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N3Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N4Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N6Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N7Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N8Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N9Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N10Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N11Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N12Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N14Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N15Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N16Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N17Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N20Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N22Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N26Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NLSSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NLGSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_SBI_XSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_S5CSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PEEMonitoredEntitySpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s TmaInventoryUnitSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AntennaInventoryUnitSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitNESpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitHwSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitSwSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitLicSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ManagedElementSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s USNFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UGWFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CGPOMUFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s iGWBFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s USCDBFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SPSV3FunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MSCServerIntraOfiSigPointSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MSCServerOfficeSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_MMESpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_HSS_MMESpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_SGSNSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_ServingGWSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_ENB_MMESpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_RPSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_RP_EPSSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SubNetworkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeHostFunctionSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s DNSRuleSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s LocationServiceSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeApplicationServiceSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeApplicationSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgePlatformServiceSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgePlatformSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RNIServiceSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s TrafficRuleSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_X2CSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_X2USpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NgCSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NgUSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_XnCSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_XnUSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_F1CSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_F1USpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_E1Spec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_S1USpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON Schema"; \
	fi

