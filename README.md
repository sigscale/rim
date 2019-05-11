# [SigScale](http://www.sigscale.org) Resource Inventory Management (RIM)

SigScale RIM is designed for communications service providers
(CSP) to manage an enterpise wide inventory of physical and logical
network resources. The initial focus is on managed objects (MO) in
support of fault and performance management (FM & PM). Fault events
and performance measurements reference MOs by distinguished name (DN).
The DN of an alarm or measurement may be looked up to get detailed
inventory and configuration information for an MO.

The information model is based on
[TM Forum](https://www.tmforum.org) 
[SID](https://www.tmforum.org/information-framework-sid/) 
and [3GPP](http://www.3gpp.org) Network Resource Models 
([NRM](https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&GSMSpecPart2=622&Search=search)).

### Graphical User Interface (GUI)
A web front end built with Google [Polymer](https://www.polymer-project.org)
web components for
[material design](https://material.io/guidelines/material-design/introduction.html) 
provides simple guided management of Resource Catalog and Inventory.
![screenshot](https://raw.githubusercontent.com/sigscale/rim/master/doc/specifications.png)

### Application Programming Interfaces (API)
The GUI provides a comfortable interface for administration however
most CSPs shall want to integrate Operations & Business Support Systems
(OSS/BSS) using machine-to-machine APIs.

#### [REST](https://en.wikipedia.org/wiki/Representational_state_transfer)
[TM Forum](https://www.tmforum.org)
[Open APIs](https://www.tmforum.org/open-apis/) are supported including:
|Title                         | Specification |
|------------------------------|---------------|
|Resource Catalog Management   | [TMF634](https://projects.tmforum.org/wiki/download/attachments/90514804/TMF634_Resource_Catalog_Management_API_REST_Specification_R17.0.1.pdf) |
|Resource Inventory Management | [TMF634](https://projects.tmforum.org/wiki/download/attachments/90514806/TMF639_Resource_Inventory_Management_REST_API_Specification_R17.0.1.pdf) |
Through TM Forum Open API polymorphic extension pattern new sub classes are defined:
|Class Name             |
|-----------------------|
|AMFFunctionSpec        |
|ASFunctionSpec         |
|BssFunctionSpec        |
|BtsSiteMgrSpec         |
|CsMgwFunctionSpec      |
|ENBFunctionSpec        |
|EPDGFunctionSpec       |
|EUtranCellFDDSpec      |
|EUtranCellTDDSpec      |
|EUtranGenericCellSpec  |
|GNBCUCPFunctionSpec    |
|GNBCUUPFunctionSpec    |
|GNBDUFunctionSpec      |
|GgsnFunctionSpec       |
|GsmCellSpec            |
|HSSFunctionSpec        |
|ICSCFFunctionSpec      |
|MMEFunctionSpec        |
|MscServerFunctionSpec  |
|NRCellCUSpec           |
|NRCellDUSpec           |
|NRSectorCarrierSpec    |
|NetworkSliceSpec       |
|NetworkSliceSubnetSpec |
|NodeBFunctionSpec      |
|PCRFFunctionSpec       |
|PCSCFFunctionSpec      |
|PGWFunctionSpec        |
|ResourceFunctionSpec   |
|RncFunctionSpec        |
|SCSCFFunctionSpec      |
|ServingGWFunctionSpec  |
|SgsnFunctionSpec       |
|UtranCellFDDSpec       |
|UtranCellTDDHcrSpec    |
|UtranCellTDDLcrSpec    |
|UtranCellTDDSpec       |
|UtranGenericCellSpec   |

#### [Erlang](http://www.erlang.org)
All operations may be performed using the Erlang public API, either
manually on the command line
[shell](http://erlang.org/doc/man/shell.html), or through custom Erlang
module development.

### [3GPP Bulk CM](https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&GSMSpecPart2=600&Search=search)
Resource inventory may be imported from element management systems 
([EMS](https://en.wikipedia.org/wiki/Element_management_system))
which support exporting Bulk CM (Configuration Management) in 3GPP TS 
[28.616](https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&GSMSpecPart2=616&Search=search) 
format XML files.

#### 3GPP Network Resource Model (NRM) Schemas
|Description |Schema              |3GPP TS|
|------------|--------------------|-------|
|Generic     |genericNrm          | 28.623|
|Generic RAN |genericRanNrm       | 28.663|
|Repeater    |repeaterNrm         | 32.796|
|GERAN       |geranNrm            | 28.656|
|UTRAN       |utranNrm            | 28.653|
|E-UTRAN     |eutranNrm           | 28.659|
|Home NodeB  |hnsNrm              | 28.673|
|Home eNodeB |hensNrm             | 28.676|
|Signaling   |stnNrm              | 28.736|
|EPC         |epcNrm              | 28.709|
|Core        |coreNrm             | 28.703|
|IMS         |imsNrm              | 28.706|
|Subscription|sumNrm              | 28.753|
|Transport   |transportNrm        | 28.733|
|SON Policy  |sonPolicyNrm        | 28.629|
|5G NR       |nrNrm               | 28.541|
|5G NR-Core  |ngcNrm              | 28.541|
|5G Slice    |sliceNrm            | 28.541|
|Bulk CM     |configData          | 32.616|
|Inventory   |inventoryNrm        | 28.633|
|Inventory   |inventoryNrmAlt2    | 28.633|
|State Mgmt  |stateManagementIRP  | 28.626|

