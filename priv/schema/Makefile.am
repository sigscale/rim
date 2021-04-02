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
		$(abs_builddir)/BssFunction.json \
		$(abs_builddir)/BtsSiteMgr.json \
		$(abs_builddir)/GsmCell.json \
		$(abs_builddir)/RncFunction.json \
		$(abs_builddir)/NodeBFunction.json \
		$(abs_builddir)/UtranGenericCellSpec.json \
		$(abs_builddir)/UtranGenericCell.json \
		$(abs_builddir)/UtranCellFDD.json \
		$(abs_builddir)/UtranCellTDDSpec.json \
		$(abs_builddir)/UtranCellTDD.json \
		$(abs_builddir)/UtranCellTDDLcr.json \
		$(abs_builddir)/UtranCellTDDHcr.json \
		$(abs_builddir)/IubLink.json \
		$(abs_builddir)/ENBFunction.json \
		$(abs_builddir)/EUtranGenericCellSpec.json \
		$(abs_builddir)/EUtranCellFDD.json \
		$(abs_builddir)/EUtranCellTDD.json \
		$(abs_builddir)/ServingGWFunction.json \
		$(abs_builddir)/PGWFunction.json \
		$(abs_builddir)/EPDGFunction.json\
		$(abs_builddir)/MMEFunction.json \
		$(abs_builddir)/PCRFFunction.json \
		$(abs_builddir)/MscServerFunction.json \
		$(abs_builddir)/CsMgwFunction.json \
		$(abs_builddir)/SgsnFunction.json \
		$(abs_builddir)/GgsnFunction.json \
		$(abs_builddir)/AucFunction.json \
		$(abs_builddir)/HlrFunction.json \
		$(abs_builddir)/EirFunction.json \
		$(abs_builddir)/MnpSrfFunction.json \
		$(abs_builddir)/CgfFunction.json \
		$(abs_builddir)/SgwFunction.json \
		$(abs_builddir)/CbcFunction.json \
		$(abs_builddir)/IucsLink.json \
		$(abs_builddir)/IupsLink.json \
		$(abs_builddir)/IubcLink.json \
		$(abs_builddir)/ALink.json \
		$(abs_builddir)/GbLink.json \
		$(abs_builddir)/NetworkSlice.json \
		$(abs_builddir)/NetworkSliceSubnet.json \
		$(abs_builddir)/AMFFunction.json \
		$(abs_builddir)/SMFFunction.json \
		$(abs_builddir)/UPFFunction.json \
		$(abs_builddir)/N3IWFFunction.json \
		$(abs_builddir)/PCFFunction.json \
		$(abs_builddir)/AUSFFunction.json \
		$(abs_builddir)/UDMFunction.json \
		$(abs_builddir)/UDRFunction.json \
		$(abs_builddir)/UDSFFunction.json \
		$(abs_builddir)/NRFFunction.json \
		$(abs_builddir)/NSSFFunction.json \
		$(abs_builddir)/SMSFFunction.json \
		$(abs_builddir)/LMFFunction.json \
		$(abs_builddir)/NGEIRFunction.json \
		$(abs_builddir)/SEPPFunction.json \
		$(abs_builddir)/NWDAFFunction.json \
		$(abs_builddir)/EP_N2.json \
		$(abs_builddir)/EP_N3.json \
		$(abs_builddir)/EP_N4.json \
		$(abs_builddir)/EP_N5.json \
		$(abs_builddir)/EP_N6.json \
		$(abs_builddir)/EP_N7.json \
		$(abs_builddir)/EP_N8.json \
		$(abs_builddir)/EP_N9.json \
		$(abs_builddir)/EP_N10.json \
		$(abs_builddir)/EP_N11.json \
		$(abs_builddir)/EP_N12.json \
		$(abs_builddir)/EP_N13.json \
		$(abs_builddir)/EP_N14.json \
		$(abs_builddir)/EP_N15.json \
		$(abs_builddir)/EP_N16.json \
		$(abs_builddir)/EP_N17.json \
		$(abs_builddir)/EP_N20.json \
		$(abs_builddir)/EP_N21.json \
		$(abs_builddir)/EP_N22.json \
		$(abs_builddir)/EP_N26.json \
		$(abs_builddir)/EP_N27.json \
		$(abs_builddir)/EP_N31.json \
		$(abs_builddir)/EP_N32.json \
		$(abs_builddir)/EP_NLS.json \
		$(abs_builddir)/EP_NLG.json \
		$(abs_builddir)/EP_SBI_X.json \
		$(abs_builddir)/EP_SBI_IPX.json \
		$(abs_builddir)/EP_S5C.json \
		$(abs_builddir)/EP_S5U.json \
		$(abs_builddir)/EP_Rx.json \
		$(abs_builddir)/EP_MAP_SMSC.json \
		$(abs_builddir)/InventoryUnit.json \
		$(abs_builddir)/TmaInventoryUnit.json \
		$(abs_builddir)/AntennaInventoryUnit.json \
		$(abs_builddir)/InventoryUnitNE.json \
		$(abs_builddir)/InventoryUnitHw.json \
		$(abs_builddir)/InventoryUnitSw.json \
		$(abs_builddir)/InventoryUnitLic.json \
		$(abs_builddir)/ManagedElementSpec.json \
		$(abs_builddir)/ManagedElement.json \
		$(abs_builddir)/USNFunction.json \
		$(abs_builddir)/UGWFunction.json \
		$(abs_builddir)/CGPOMUFunction.json \
		$(abs_builddir)/iGWBFunction.json \
		$(abs_builddir)/USCDBFunction.json \
		$(abs_builddir)/SPSV3Function.json \
		$(abs_builddir)/MSCServerIntraOfiSigPoint.json \
		$(abs_builddir)/MSCServerOffice.json \
		$(abs_builddir)/Link_MME_MME.json \
		$(abs_builddir)/Link_HSS_MME.json \
		$(abs_builddir)/Link_MME_SGSN.json \
		$(abs_builddir)/Link_MME_ServingGW.json \
		$(abs_builddir)/Link_ENB_MME.json \
		$(abs_builddir)/EP_RPSpec.json \
		$(abs_builddir)/EP_RP.json \
		$(abs_builddir)/EP_RP_EPS.json \
		$(abs_builddir)/SubNetworkSpec.json \
		$(abs_builddir)/SubNetwork.json \
		$(abs_builddir)/MobileEdgeHostFunction.json \
		$(abs_builddir)/DNSRule.json \
		$(abs_builddir)/LocationService.json \
		$(abs_builddir)/MobileEdgeApplicationService.json \
		$(abs_builddir)/MobileEdgeApplication.json \
		$(abs_builddir)/MobileEdgePlatformService.json \
		$(abs_builddir)/MobileEdgePlatform.json \
		$(abs_builddir)/RNIService.json \
		$(abs_builddir)/TrafficRule.json \
		$(abs_builddir)/API.json \
		$(abs_builddir)/ApiSpecification.json \
		$(abs_builddir)/HostingPlatformRequirement.json \
		$(abs_builddir)/HostingPlatformRequirementSpecification.json \
		$(abs_builddir)/InstalledSoftware.json \
		$(abs_builddir)/SoftwareResource.json \
		$(abs_builddir)/SoftwareResourceSpecification.json \
		$(abs_builddir)/SoftwareSpecification.json \
		$(abs_builddir)/SoftwareSupportPackage.json \
		$(abs_builddir)/resourceCatalogManagement.json \
		$(abs_builddir)/resourceFunctionActivation.json \
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
		if ! ajv compile -s resourceFunctionActivation.json \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SoftwareResourceSpecification.json; \
			then exit 1; fi; \
		if ! ajv compile -s SoftwareSpecification.json; \
			then exit 1; fi; \
		if ! ajv compile -s ApiSpecification.json; \
			then exit 1; fi; \
		if ! ajv compile -s HostingPlatformRequirementSpecification.json; \
			then exit 1; fi; \
		if ! ajv compile -s SoftwareResource.json; \
			then exit 1; fi; \
		if ! ajv compile -s InstalledSoftware.json; \
			then exit 1; fi; \
		if ! ajv compile -s API.json; \
			then exit 1; fi; \
		if ! ajv compile -s HostingPlatformRequirement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SoftwareSupportPackage.json; \
			then exit 1; fi; \
		if ! ajv compile -s ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BssFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s BtsSiteMgr.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GsmCell.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RncFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NodeBFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranGenericCellSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranGenericCell.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellFDD.json \
			-r UtranGenericCell.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDSpec.json \
			-r UtranGenericCellSpec \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDD.json \
			-r UtranGenericCell.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDLcr.json \
			-r UtranGenericCell.json \
			-r UtranCellTDD.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UtranCellTDDHcr.json \
			-r UtranGenericCell.json \
			-r UtranCellTDD.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IubLink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ENBFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranGenericCellSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellFDD.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EUtranCellTDD.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ServingGWFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PGWFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EPDGFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MMEFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCRFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MscServerFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CsMgwFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgsnFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GgsnFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AucFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s HlrFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EirFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MnpSrfFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CgfFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SgwFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CbcFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IucsLink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IupsLink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s IubcLink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ALink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s GbLink.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSlice.json \
			-r SubNetwork.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NetworkSliceSubnet.json \
			-r SubNetwork.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AMFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SMFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UPFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s N3IWFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s PCFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AUSFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDMFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDRFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UDSFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NRFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NSSFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SMSFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s LMFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NGEIRFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SEPPFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s NWDAFFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N2.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N3.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N4.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N5.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N6.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N7.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N8.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N9.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N10.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N11.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N12.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N13.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N14.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N15.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N16.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N17.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N20.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N21.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N22.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N26.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N27.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N31.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_N32.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NLS.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NLG.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_SBI_X.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_SBI_IPX.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_S5C.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_S5U.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_Rx.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_MAP_SMSC.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnit.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s TmaInventoryUnit.json \
			-r InventoryUnit.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s AntennaInventoryUnit.json \
			-r InventoryUnit.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitNE.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitHw.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitSw.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s InventoryUnitLic.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ManagedElementSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s ManagedElement.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s USNFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s UGWFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s CGPOMUFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s iGWBFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s USCDBFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SPSV3Function.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MSCServerIntraOfiSigPoint.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MSCServerOffice.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_MME.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_HSS_MME.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_SGSN.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_MME_ServingGW.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s Link_ENB_MME.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_RPSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_RP_EPS.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SubNetworkSpec.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceCatalogManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s SubNetwork.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeHostFunction.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s DNSRule.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s LocationService.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeApplicationService.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgeApplication.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgePlatformService.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s MobileEdgePlatform.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s RNIService.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s TrafficRule.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_X2C.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_X2U.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NgC.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_NgU.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_XnC.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_XnU.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_F1C.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_F1U.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_E1.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
		if ! ajv compile -s EP_S1U.json \
			-r EP_RP.json \
			-r ManagedFunction.json \
			-r genericNrm.json \
			--unknown-formats="base64" \
			--unknown-formats="float" \
			-r resourceFunctionActivation.json \
			-r resourceInventoryManagement.json; \
			then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON Schema"; \
	fi

