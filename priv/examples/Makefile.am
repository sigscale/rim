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
		$(abs_builddir)/specification-epc-mme.json \
		$(abs_builddir)/specification-epc-pcrf.json \
		$(abs_builddir)/specification-core-msc.json \
		$(abs_builddir)/specification-core-mgw.json \
		$(abs_builddir)/specification-core-sgsn.json \
		$(abs_builddir)/specification-core-ggsn.json \
		$(abs_builddir)/specification-ims-as.json \
		$(abs_builddir)/specification-ims-hss.json \
		$(abs_builddir)/specification-ims-pcscf.json \
		$(abs_builddir)/specification-ims-scscf.json \
		$(abs_builddir)/specification-ims-icscf.json \
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
				-d specification-gsm-bss.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/BtsSiteMgrSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-gsm-bts.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GsmCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-gsm-cell.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/RncFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-umts-rnc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/NodeBFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-umts-nodeb.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellFDDSpecification.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-umts-cell-fdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellTDDLcrSpecification.json \
				-r $(top_builddir)/priv/schema/UtranCellTDDSpecification.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-umts-cell-tdd-lcr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/UtranCellTDDHcrSpecification.json \
				-r $(top_builddir)/priv/schema/UtranCellTDDSpecification.json \
				-r $(top_builddir)/priv/schema/UtranGenericCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-umts-cell-tdd-hcr.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ENBFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-lte-enb.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EUtranCellFDDSpecification.json \
				-r $(top_builddir)/priv/schema/EUtranGenericCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-lte-cell-fdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/EUtranCellTDDSpecification.json \
				-r $(top_builddir)/priv/schema/EUtranGenericCellSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-lte-cell-tdd.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ServingGWFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-epc-sgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PGWFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-epc-pgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/MMEFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-epc-mme.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PCRFFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-epc-pcrf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/MscServerFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-core-msc.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/CsMgwFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-core-mgw.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SgsnFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-core-sgsn.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GgsnFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-core-ggsn.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/GgsnFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-core-ggsn.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ASFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-ims-as.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/HSSFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-ims-hss.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/PCSCFFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-ims-pcscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/SCSCFFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-ims-scscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/ICSCFFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/ResourceFunctionSpecification.json \
				-r $(top_builddir)/priv/schema/resourceCatalogManagement.json \
				-d specification-ims-icscf.json; then exit 1; fi; \
		if ! ajv validate -s $(top_builddir)/priv/schema/resourceInventoryManagement.json \
				-d resource.json; then exit 1; fi; \
	else \
		echo "ajv-cli needed to validate JSON"; \
	fi

