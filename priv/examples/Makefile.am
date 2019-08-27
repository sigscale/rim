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
		$(abs_builddir)/peeCmon.xml \
		$(abs_builddir)/epcn3ai.xml \
		$(abs_builddir)/catalog.json \
		$(abs_builddir)/category.json \
		$(abs_builddir)/candidate.json \
		$(abs_builddir)/specification-gsm-bss.json \
		$(abs_builddir)/specification-gsm-bts.json \
		$(abs_builddir)/specification-gsm-cell.json \
		$(abs_builddir)/specification-umts-rnc.json \
		$(abs_builddir)/specification-umts-nodeb.json \
		$(abs_builddir)/specification-umts-cell-fdd.json \
		$(abs_builddir)/specification-umts-cell-tdd-lcr.json \
		$(abs_builddir)/specification-umts-cell-tdd-hcr.json \
		$(abs_builddir)/specification-lte-enb.json \
		$(abs_builddir)/specification-lte-cell-fdd.json \
		$(abs_builddir)/specification-lte-cell-tdd.json \
		$(abs_builddir)/specification-epc-sgw.json \
		$(abs_builddir)/specification-epc-pgw.json \
		$(abs_builddir)/specification-epc-epdg.json \
		$(abs_builddir)/specification-epc-mme.json \
		$(abs_builddir)/specification-epc-pcrf.json \
		$(abs_builddir)/specification-core-msc.json \
		$(abs_builddir)/specification-core-mgw.json \
		$(abs_builddir)/specification-core-sgsn.json \
		$(abs_builddir)/specification-core-ggsn.json \
		$(abs_builddir)/specification-core-auc.json \
		$(abs_builddir)/specification-core-hlr.json \
		$(abs_builddir)/specification-core-eir.json \
		$(abs_builddir)/specification-core-mnpsrf.json \
		$(abs_builddir)/specification-core-cgf.json \
		$(abs_builddir)/specification-ims-as.json \
		$(abs_builddir)/specification-ims-hss.json \
		$(abs_builddir)/specification-ims-pcscf.json \
		$(abs_builddir)/specification-ims-scscf.json \
		$(abs_builddir)/specification-ims-icscf.json \
		$(abs_builddir)/specification-nr-gnb-du.json \
		$(abs_builddir)/specification-nr-gnb-cu-cp.json \
		$(abs_builddir)/specification-nr-gnb-cu-up.json \
		$(abs_builddir)/specification-nr-cell-cu.json \
		$(abs_builddir)/specification-nr-cell-du.json \
		$(abs_builddir)/specification-nr-sector-carrier.json \
		$(abs_builddir)/specification-5gc-slice.json \
		$(abs_builddir)/specification-5gc-slice-subnet.json \
		$(abs_builddir)/specification-5gc-amf.json \
		$(abs_builddir)/specification-5gc-smf.json \
		$(abs_builddir)/specification-5gc-upf.json \
		$(abs_builddir)/specification-5gc-n3iwf.json \
		$(abs_builddir)/specification-5gc-pcf.json \
		$(abs_builddir)/specification-5gc-ausf.json \
		$(abs_builddir)/specification-5gc-udm.json \
		$(abs_builddir)/specification-5gc-udr.json \
		$(abs_builddir)/specification-5gc-udsf.json \
		$(abs_builddir)/specification-5gc-nrf.json \
		$(abs_builddir)/specification-5gc-nssf.json \
		$(abs_builddir)/specification-5gc-smsf.json \
		$(abs_builddir)/specification-5gc-lmf.json \
		$(abs_builddir)/specification-5gc-ngeir.json \
		$(abs_builddir)/specification-5gc-sepp.json \
		$(abs_builddir)/specification-5gc-nwdaf.json \
		$(abs_builddir)/specification-pee-me.json \
		$(abs_builddir)/resource-gsm-bss.json

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
		if ! ajv validate -s $(top_builddir)/priv/schema/BssFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-gsm-bss.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/BtsSiteMgrSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-gsm-bts.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GsmCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-gsm-cell.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/RncFunctionSpec.json \
				-r $(top_builddir)/priv/schema/genericNrm.json \
				-r $(top_builddir)/priv/schema/utranNrm.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-umts-rnc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NodeBFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-umts-nodeb.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellFDDSpec.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-umts-cell-fdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellTDDLcrSpec.json \
				-r $(top_builddir)/priv/schema/UtranCellTDDSpec.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-umts-cell-tdd-lcr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellTDDHcrSpec.json \
				-r $(top_builddir)/priv/schema/UtranCellTDDSpec.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-umts-cell-tdd-hcr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ENBFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-lte-enb.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EUtranCellFDDSpec.json \
				-r $(top_builddir)/priv/schema/EUtranGenericCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-lte-cell-fdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EUtranCellTDDSpec.json \
				-r $(top_builddir)/priv/schema/EUtranGenericCellSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-lte-cell-tdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ServingGWFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-epc-sgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PGWFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-epc-pgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EPDGFunctionSpec.json\
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-epc-epdg.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/MMEFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-epc-mme.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PCRFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-epc-pcrf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/MscServerFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-msc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/CsMgwFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-mgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SgsnFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-sgsn.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GgsnFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-ggsn.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/AucFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-auc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/HlrFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-hlr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EirFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-eir.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/MnpSrfFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-mnpsrf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/CgfFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-core-cgf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ASFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-ims-as.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/HSSFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-ims-hss.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PCSCFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-ims-pcscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SCSCFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-ims-scscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ICSCFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-ims-icscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GNBDUFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-gnb-du.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GNBCUCPFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-gnb-cu-cp.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GNBCUUPFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-gnb-cu-up.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NRCellCUSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-cell-cu.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NRCellDUSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-cell-du.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NRSectorCarrierSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-nr-sector-carrier.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NetworkSliceSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-slice.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NetworkSliceSubnetSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-slice-subnet.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/AMFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-amf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SMFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-smf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UPFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-upf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/N3IWFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-n3iwf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PCFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-pcf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/AUSFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-ausf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UDMFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-udm.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UDRFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-udr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UDSFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-udsf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NRFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-nrf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NSSFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-nssf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SMSFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-smsf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/LMFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-lmf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NGEIRFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-ngeir.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SEPPFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-sepp.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NWDAFFunctionSpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-5gc-nwdaf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PEEMonitoredEntitySpec.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpec.json \
				-r $(top_builddir)/priv/schema/entityCatalogManagement.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-r $(top_builddir)/priv/schema/SAPSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectionPointSpecificationRef.json \
				-r $(top_builddir)/priv/schema/ConnectivitySpecification.json \
				-d specification-pee-me.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceInventoryManagement.json \
				-d resource-gsm-bss.json; then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON"; \
	fi

