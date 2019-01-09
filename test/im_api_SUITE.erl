%%% im_api_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test suite for the public API of the
%%% {@link //sigscale_im. sigscale_im} application.
%%%
-module(im_api_SUITE).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("im.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("inets/include/mod_auth.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = im_test_lib:initialize_db(),
	ok = im_test_lib:start(),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = application:stop(sigscale_im),
	ok = application:stop(mnesia).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(bulk_cm_generic, Config) ->
	NumResources = 10,
	Newline = #xmlText{value = "\n",type = text},
	Indent1 = #xmlText{value = "\n\t",type = text},
	Indent2 = #xmlText{value = "\n\t\t",type = text},
	Indent3 = #xmlText{value = "\n\t\t\t",type = text},
	Indent4 = #xmlText{value = "\n\t\t\t\t",type = text},
	Indent5 = #xmlText{value = "\n\t\t\t\t\t",type = text},
	Indent6 = #xmlText{value = "\n\t\t\t\t\t\t",type = text},
	F = fun F(0, Acc) ->
				[Indent3, {'xn:ManagementNode', [{id, "1"}],
						[Indent4, {'xn:attributes', [],
								[Indent5, {'xn:userLabel', ["Paris MN1"]},
								Indent5, {'xn:vendorName', ["Company NN"]},
								Indent5, {'xn:userDefinedState', ["commercial"]},
								Indent5, {'xn:locationName', ["Montparnasse"]},
								Indent5, {'xn:managedElements', [],
										[Indent6, {'xn:dn', ["ManagementNode=1,managedElement=1"]},
										Indent6, {'xn:dn', ["ManagementNode=1,managedElement=2"]}, Indent6]},
								Indent5, {'xn:swVersion', ["1.0"]}, Indent4]}, Indent3]} | Acc];
			F(N, Acc) ->
				Resource = {'xn:ManagedElement', [{id, integer_to_list(N)}],
						[Indent4, {'xn:attributes', [],
								[Indent5, {'xn:dnPrefix', ["SubNetwork=1,ManagedElement=" ++ integer_to_list(N)]},
								Indent5, {'xn:managedElementTypeList', [],
										[Indent6, {'xn:managedElementType', ["RNC"]}, Indent6]},
								Indent5, {'xn:userLabel', [generate_identity(10)]},
								Indent5, {'xn:vendorName', [generate_identity(10)]},
								Indent5, {'xn:userDefinedState', ["commercial"]},
								Indent5, {'xn:locationName', [generate_identity(10)]},
								Indent5, {'xn:swVersion', ["1.0"]},
								Indent5, {'xn:managedBy', [],
										[Indent6, {'xn:dn', ["ManagementNode=1,ManagementElement=" ++
												integer_to_list(N)]}, Indent5]}, Indent4]}, Indent3]},
				F(N - 1, [Indent3, Resource | Acc])
	end,
	Resources = F(NumResources, []),
	FileAttributes = [{xmlns, "http://www.3gpp.org/ftp/specs/archive/32_series/32.616#configData"},
			{'xmlns:xn', "http://www.3gpp.org/ftp/specs/archive/32_series/32.626#genericNrm"}],
	FileHeader = {fileHeader, [{fileFormatVersion, "32.616 V15.0"},
			{senderName, "DC=a1.companyNN.com,SubNetwork=1,IRPAgent=1"},
			{vendorName, "Company NN"}], []},
	ConfigData = {configData, [{dnPrefix, "DC=a1.companyNN.com"}],
			[Indent2, {'xn:SubNetwork', [{id, "1"}],
					[Indent3, {'xn:attributes', [],
							[Indent4, {'xn:dnPrefix', ["SubNetwork=1"]},
							Indent4, {'xn:userLabel', ["Paris SN1"]},
							Indent4, {'xn:userDefinedNetworkType', ["UMTS"]},
							Indent4, {'xn:setOfMcc', []}, Indent3]} | Resources] ++ [Indent2]}, Indent1]},
	FileFooter = {fileFooter, [{dateTime, "2018-12-15T12:00:00Z"}], []},
	Simple = [Newline, {bulkCmConfigDataFile, FileAttributes,
			[Indent1, FileHeader, Indent1, ConfigData, Indent1, FileFooter, Newline]}],
	GenericNrmXML = lists:flatten(xmerl:export_simple(Simple, xmerl_xml, [])),
	PrivDir = ?config(priv_dir, Config),
	XMLPath = PrivDir ++ "/" ++ "genericNrm.xml",
	file:write_file(XMLPath, GenericNrmXML),
	Config;
init_per_testcase(bulk_cm_geran, Config) ->
	NumResources = 10,
	Newline = #xmlText{value = "\n",type = text},
	Indent1 = #xmlText{value = "\n\t",type = text},
	Indent2 = #xmlText{value = "\n\t\t",type = text},
	Indent3 = #xmlText{value = "\n\t\t\t",type = text},
	Indent4 = #xmlText{value = "\n\t\t\t\t",type = text},
	Indent5 = #xmlText{value = "\n\t\t\t\t\t",type = text},
	Indent6 = #xmlText{value = "\n\t\t\t\t\t\t",type = text},
	F = fun F(0, Acc) ->
				[Indent3, {'gn:BtsSiteMgr', [{id, "1"}],
						[Indent4, {'gn:attributes', [],
								[Indent5, {'gn:userLabel', ["Paris MN2"]},
								Indent5, {'gn:vnfParametersList', [],
										[Indent6, {'xn:vnfInstanceId', [generate_identity(7)]},
										Indent6, {'xn:vnfdId', [generate_identity(7)]},
										Indent6, {'xn:flavourId', [generate_identity(7)]},
										Indent6, {'xn:autoScalable', ["true"]}, Indent5]},
								Indent5, {'gn:operationalState', ["Disabled"]},
										Indent4]}, Indent3]} | Acc];
			F(N, Acc) ->
				Resource = {'xn:VsDataContainer', [{id, integer_to_list(N)}],
						[Indent4, {'xn:attributes', [],
								[Indent5, {'xn:vsDataType', ["DataType " ++ integer_to_list(N)]},
								Indent5, {'xn:vsDataFormatVersion', [generate_identity(5)]},
								Indent5, {'xn:vsData', []}, Indent4]}, Indent3]},
				F(N - 1, [Indent3, Resource | Acc])
	end,
	Resources = F(NumResources, []),
	FileAttributes = [{xmlns, "http://www.3gpp.org/ftp/specs/archive/32_series/32.616#configData"},
			{'xmlns:xn', "http://www.3gpp.org/ftp/specs/archive/28_series/28.623#genericNrm"},
			{'xmlns:gn', "http://www.3gpp.org/ftp/specs/archive/28_series/28.656#geranNrm"}],
	FileHeader = {fileHeader, [{fileFormatVersion, "32.616 V15.0"},
			{senderName, "DC=a1.companyNN.com,SubNetwork=1,IRPAgent=1"},
			{vendorName, "Company NN"}], []},
	ConfigData = {configData, [{dnPrefix, "DC=a1.companyNN.com"}],
			[Indent2, {'gn:BssFunction', [{id, "1"}],
					[Indent3, {'xn:attributes', [],
							[Indent4, {'gn:userLabel', ["BssFunction=1"]},
							Indent4, {'gn:vnfParametersList', [],
									[Indent5, {'xn:vnfInstanceId', [generate_identity(7)]},
									Indent5, {'xn:vnfdId', [generate_identity(7)]},
									Indent5, {'xn:flavourId', [generate_identity(7)]},
									Indent5, {'xn:autoScalable', ["true"]}, Indent4]},
											Indent3]} | Resources] ++ [Indent2]}, Indent1]},
	FileFooter = {fileFooter, [{dateTime, "2018-12-26T12:00:00Z"}], []},
	Simple = [Newline, {bulkCmConfigDataFile, FileAttributes,
			[Indent1, FileHeader, Indent1, ConfigData, Indent1, FileFooter, Newline]}],
	GeranNrmXML = lists:flatten(xmerl:export_simple(Simple, xmerl_xml, [])),
	PrivDir = ?config(priv_dir, Config),
	XMLPath = PrivDir ++ "/" ++ "geranNrm.xml",
	file:write_file(XMLPath, GeranNrmXML),
	Config;
init_per_testcase(bulk_cm_utran, Config) ->
	NumResources = 10,
	Newline = #xmlText{value = "\n",type = text},
	Indent1 = #xmlText{value = "\n\t",type = text},
	Indent2 = #xmlText{value = "\n\t\t",type = text},
	Indent3 = #xmlText{value = "\n\t\t\t",type = text},
	Indent4 = #xmlText{value = "\n\t\t\t\t",type = text},
	Indent5 = #xmlText{value = "\n\t\t\t\t\t",type = text},
	Indent6 = #xmlText{value = "\n\t\t\t\t\t\t",type = text},
	F = fun F(0, Acc) ->
				[Indent3, {'un:UtranCellFDD', [{id, "1"}],
						[Indent4, {'un:attributes', [],
								[Indent5, {'un:userLabel', ["Paris MN7"]},
								Indent5, {'un:localCellId', ["268435455"]},
								Indent5, {'un:cellMode', ["3-84McpsTDDMode"]},
								Indent5, {'un:fachPower', ["150"]},
								Indent5, {'un:relatedAntennaList', [],
										[Indent6, {'xn:dn', ["RncFunction=1,UtranCellFDD=1"]},
										Indent6, {'xn:dn', ["RncFunction=1,UtranCellFDD=2"]}, Indent5]},
								Indent5, {'un:operationalState', ["disabled"]},
								Indent5, {'un:numOfHspdschs', ["0"]}, Indent4]}, Indent3]} | Acc];
			F(N, Acc) ->
				Resource = {'un:UtranCellTDDLcr', [{id, integer_to_list(N)}],
						[Indent4, {'un:attributes', [],
								[Indent5, {'un:userLabel', [generate_identity(10)]},
								Indent5, {'un:cellMode', ["1-28McpsTDDMode"]},
								Indent5, {'un:rac', ["205"]},
								Indent5, {'un:operationalState', ["disabled"]},
								Indent5, {'un:hsEnable', ["1"]},
								Indent5, {'un:snaInformation', ["990"]},
								Indent5, {'un:relatedSectorEquipment', [generate_identity(10)]},
								Indent5, {'un:primaryCcpchPower', ["-10"]},
								Indent5, {'un:dwPchPower', ["30"]}, Indent4]}, Indent3]},
				F(N - 1, [Indent3, Resource | Acc])
	end,
	Resources = F(NumResources, []),
	FileAttributes = [{xmlns, "http://www.3gpp.org/ftp/specs/archive/32_series/32.616#configData"},
			{'xmlns:xn', "http://www.3gpp.org/ftp/specs/archive/28_series/28.623#genericNrm"},
			{'xmlns:un', "http://www.3gpp.org/ftp/specs/archive/28_series/28.653#utranNrm"}],
	FileHeader = {fileHeader, [{fileFormatVersion, "32.616 V15.0"},
			{senderName, "DC=a1.companyNN.com,SubNetwork=1,IRPAgent=1"},
			{vendorName, "Company NN"}], []},
	ConfigData = {configData, [{dnPrefix, "DC=a1.companyNN.com"}],
			[Indent2, {'un:RncFunction', [{id, "1"}],
					[Indent3, {'un:attributes', [],
							[Indent4, {'un:userLabel', ["Paris SN5"]},
							Indent5, {'gn:vnfParametersList', [],
									[Indent6, {'xn:vnfInstanceId', [generate_identity(7)]},
									Indent6, {'xn:vnfdId', [generate_identity(7)]},
									Indent6, {'xn:flavourId', [generate_identity(7)]},
									Indent6, {'xn:autoScalable', ["true"]}, Indent5]},
							Indent4, {'un:mcc', ["MCC"]},
							Indent4, {'un:siptoSupported', ["1"]},
							Indent4, {'un:tceIDMappingInfoList', [],
								[Indent5, {'un:tceIDMappingInfo', [],
									[Indent6, {'un:tceID', ["1995"]},
									Indent6, {'un:tceIPAddr', ["Colombo"]},
											Indent5]}, Indent4]}, Indent3]} | Resources] ++ [Indent2]}, Indent1]},
	FileFooter = {fileFooter, [{dateTime, "2018-12-27T12:00:00Z"}], []},
	Simple = [Newline, {bulkCmConfigDataFile, FileAttributes,
         [Indent1, FileHeader, Indent1, ConfigData, Indent1, FileFooter, Newline]}],
	UtranNrmXML = lists:flatten(xmerl:export_simple(Simple, xmerl_xml, [])),
	PrivDir = ?config(priv_dir, Config),
	XMLPath = PrivDir ++ "/" ++ "utranNrm.xml",
	file:write_file(XMLPath, UtranNrmXML),
	Config;
init_per_testcase(bulk_cm_eutran, Config) ->
	NumResources = 10,
	Newline = #xmlText{value = "\n",type = text},
	Indent1 = #xmlText{value = "\n\t",type = text},
	Indent2 = #xmlText{value = "\n\t\t",type = text},
	Indent3 = #xmlText{value = "\n\t\t\t",type = text},
	Indent4 = #xmlText{value = "\n\t\t\t\t",type = text},
	Indent5 = #xmlText{value = "\n\t\t\t\t\t",type = text},
	Indent6 = #xmlText{value = "\n\t\t\t\t\t\t",type = text},
	Indent7 = #xmlText{value = "\n\t\t\t\t\t\t\t",type = text},
	Indent8 = #xmlText{value = "\n\t\t\t\t\t\t\t\t",type = text},
	F = fun F(0, Acc) ->
				[Indent3, {'en:DeNBCapability', [{id, "1"}],
						[Indent4, {'en:attributes', [],
								[Indent5, {'en:userLabel', ["Orang MNO1"]},
								Indent5, {'en:vnfParametersList', [],
										[Indent6, {'xn:vnfInstanceId', [generate_identity(7)]},
										Indent6, {'xn:vnfdId', [generate_identity(7)]},
										Indent6, {'xn:flavourId', [generate_identity(7)]},
										Indent6, {'xn:autoScalable', ["true"]}, Indent5]},
								Indent5, {'en:maxNbrRNAllowed', ["853"]},
								Indent5, {'en:servedRN', [],
									[Indent6, {'xn:dn', ["ENBFunction=1,DeNBCapability=1"]},
									Indent6, {'xn:dn', ["ENBFunction=1,DeNBCapability=2"]},
											Indent5]}, Indent4]}, Indent3]} | Acc];
			F(N, Acc) ->
				Resource = {'en:EUtranCellTDD', [{id, integer_to_list(N)}],
						[Indent4, {'en:attributes', [],
								[Indent5, {'en:userLabel', [generate_identity(10)]},
								Indent5, {'en:cellLocalId', ["211"]},
								Indent5, {'en:cellSize', ["verysmall"]},
								Indent5, {'en:pLMNIdList', [],
									[Indent6, {'en:pLMNId', []}, Indent5]},
								Indent5, {'en:cellAccessInfoList', [],
									[Indent6, {'en:cellAccessInfo', [],
											[Indent7, {'en:plmnId', [],
													[Indent8, {'en:mcc', ["145"]},
													Indent8, {'en:mnc', ["456"]}, Indent7]},
											Indent7, {'en:tac', ["795645365"]},
											Indent7, {'en:cellId', ["65987134"]}, Indent6]}, Indent5]},
								Indent5, {'en:tac', ["965213215"]},
								Indent5, {'en:pci', ["450"]},
								Indent5, {'en:maximumTransmissionPower', ["685"]},
								Indent5, {'en:referenceSignalPower', ["267"]},
								Indent5, {'en:pb', ["198"]},
								Indent5, {'en:allowedAccessClasses', [],
									[Indent6, {'en:allowedAccessClassesElement', ["SecurityServices"]}, Indent5]},
								Indent5, {'en:earfcn', ["852"]},
								Indent5, {'en:sfAssignment', ["635"]},
								Indent5, {'en:specialSfPatterns', ["753"]}, Indent4]}, Indent3]},
				F(N - 1, [Indent3, Resource | Acc])
	end,
	Resources = F(NumResources, []),
	FileAttributes = [{xmlns, "http://www.3gpp.org/ftp/specs/archive/32_series/32.616#configData"},
			{'xmlns:xn', "http://www.3gpp.org/ftp/specs/archive/28_series/28.623#genericNrm"},
			{'xmlns:en', "http://www.3gpp.org/ftp/specs/archive/32_series/32.766#eutranNrm"}],
	FileHeader = {fileHeader, [{fileFormatVersion, "32.616 V15.0"},
			{senderName, "DC=a1.companyNN.com,SubNetwork=1,IRPAgent=1"},
			{vendorName, "Company NN"}], []},
	ConfigData = {configData, [{dnPrefix, "DC=a1.companyNN.com"}],
			[Indent2, {'en:ENBFunction', [{id, "1"}],
					[Indent3, {'en:attributes', [],
							[Indent4, {'en:userLabel', ["France SN1"]},
							Indent4, {'en:iRATANRSwitch', ["true"]},
							Indent4, {'en:x2BlackList', [],
								[Indent5, {'xn:dn', ["ENBFunction=1,x2BlackList"]}, Indent4]},
							Indent4, {'en:x2HOBlackList', [],
								[Indent5, {'xn:dn', ["ENBFunction=1,x2HOBlackList"]}, Indent4]},
							Indent4, {'en:tceIDMappingInfoList', [],
								[Indent5, {'en:tceIDMappingInfo', []},
										Indent4]}, Indent3]} | Resources] ++ [Indent2]}, Indent1]},
	FileFooter = {fileFooter, [{dateTime, "2018-12-27T18:00:00Z"}], []},
	Simple = [Newline, {bulkCmConfigDataFile, FileAttributes,
			[Indent1, FileHeader, Indent1, ConfigData, Indent1, FileFooter, Newline]}],
	EutranNrmXML = lists:flatten(xmerl:export_simple(Simple, xmerl_xml, [])),
	PrivDir = ?config(priv_dir, Config),
	XMLPath = PrivDir ++ "/" ++ "eutranNrm.xml",
	file:write_file(XMLPath, EutranNrmXML),
	Config;
init_per_testcase(bulk_cm_epc, Config) ->
	NumResources = 10,
	Newline = #xmlText{value = "\n",type = text},
	Indent1 = #xmlText{value = "\n\t",type = text},
	Indent2 = #xmlText{value = "\n\t\t",type = text},
	Indent3 = #xmlText{value = "\n\t\t\t",type = text},
	Indent4 = #xmlText{value = "\n\t\t\t\t",type = text},
	Indent5 = #xmlText{value = "\n\t\t\t\t\t",type = text},
	Indent6 = #xmlText{value = "\n\t\t\t\t\t\t",type = text},
	F = fun F(0, Acc) ->
				[Indent3, {'epc:EP_RP_EPS', [{id, "1"}],
						[Indent4, {'epc:attributes', [],
								[Indent5, {'epc:farEndEntity', ["FEE"]},
								Indent5, {'epc:userLabel', [generate_identity(10)]}, Indent4]}, Indent3]} | Acc];
			F(N, Acc) ->
				Resource = {'xn:VsDataContainer', [{id, integer_to_list(N)}],
						[Indent4, {'xn:attributes', [],
								[Indent5, {'xn:vsDataType', ["DataType " ++ integer_to_list(N)]},
								Indent5, {'xn:vsDataFormatVersion', [generate_identity(5)]},
								Indent5, {'xn:vsData', []}, Indent4]}, Indent3]},
				F(N - 1, [Indent3, Resource | Acc])
	end,
	Resources = F(NumResources, []),
	FileAttributes = [{xmlns, "http://www.3gpp.org/ftp/specs/archive/32_series/32.616#configData"},
			{'xmlns:xn', "http://www.3gpp.org/ftp/specs/archive/28_series/28.623#genericNrm"},
			{'xmlns:epc', "http://www.3gpp.org/ftp/specs/archive/28_series/28.709#epcNrm"}],
	FileHeader = {fileHeader, [{fileFormatVersion, "32.616 V15.0"},
			{senderName, "DC=a1.companyNN.com,SubNetwork=1,IRPAgent=1"},
			{vendorName, "Company NN"}], []},
	ConfigData = {configData, [{dnPrefix, "DC=a1.companyNN.com"}],
			[Indent2, {'epc:MMEFunction', [{id, "1"}],
					[Indent3, {'epc:attributes', [],
							[Indent4, {'epc:userLabel', ["Paris SMN1"]},
							Indent4, {'epc:pLMNIdList', [],
								[Indent5, {'epc:pLMNId', [],
									[Indent6, {'epc:mcc', ["126"]},
									Indent6, {'epc:mNc', ["852"]}, Indent5]}, Indent4]},
							Indent4, {'epc:mMEC', ["145985"]},
							Indent4, {'epc:mMEPool', ["MMEP"]}, Indent3]} | Resources] ++ [Indent2]}, Indent1]},
	FileFooter = {fileFooter, [{dateTime, "2018-12-27T23:00:00Z"}], []},
	Simple = [Newline, {bulkCmConfigDataFile, FileAttributes,
			[Indent1, FileHeader, Indent1, ConfigData, Indent1, FileFooter, Newline]}],
	EpcNrmXML = lists:flatten(xmerl:export_simple(Simple, xmerl_xml, [])),
	PrivDir = ?config(priv_dir, Config),
	XMLPath = PrivDir ++ "/" ++ "epcNrm.xml",
	file:write_file(XMLPath, EpcNrmXML),
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[bulk_cm_generic, bulk_cm_geran, bulk_cm_utran, bulk_cm_eutran, bulk_cm_epc].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

bulk_cm_generic() ->
	[{userdata, [{doc, "Import bulk CM for generic network resources"}]}].

bulk_cm_generic(Config) ->
	PrivDir = ?config(priv_dir, Config),
	GenericXML = PrivDir ++ "/" ++ "genericNrm.xml",
	ok = cm:import(GenericXML),
	[ConfigData] = decode_xml(xmerl_scan:file(GenericXML)),
	Resources = get_resources(ConfigData#xmlElement.content),
	ResourceNames = [Name || #xmlElement{name = Name} <- Resources],
	ok = check_resources(ResourceNames).

bulk_cm_geran() ->
	[{userdata, [{doc, "Import bulk CM for geran network resources"}]}].

bulk_cm_geran(Config) ->
	PrivDir = ?config(priv_dir, Config),
	GeranXML = PrivDir ++ "/" ++ "geranNrm.xml",
	ok = cm:import(GeranXML),
	[ConfigData] = decode_xml(xmerl_scan:file(GeranXML)),
	Resources = get_resources(ConfigData#xmlElement.content),
	ResourceNames = [Name || #xmlElement{name = Name} <- Resources],
	ok = check_resources(ResourceNames).

bulk_cm_utran() ->
	[{userdata, [{doc, "Import bulk CM for utran network resources"}]}].

bulk_cm_utran(Config) ->
	PrivDir = ?config(priv_dir, Config),
	UtranXML = PrivDir ++ "/" ++ "utranNrm.xml",
	ok = cm:import(UtranXML),
	[ConfigData] = decode_xml(xmerl_scan:file(UtranXML)),
	Resources = get_resources(ConfigData#xmlElement.content),
	ResourceNames = [Name || #xmlElement{name = Name} <- Resources],
	ok = check_resources(ResourceNames).

bulk_cm_eutran() ->
	[{userdata, [{doc, "Import bulk CM for eutran network resources"}]}].

bulk_cm_eutran(Config) ->
	PrivDir = ?config(priv_dir, Config),
	EutranXML = PrivDir ++ "/" ++ "eutranNrm.xml",
	ok = cm:import(EutranXML),
	[ConfigData] = decode_xml(xmerl_scan:file(EutranXML)),
	Resources = get_resources(ConfigData#xmlElement.content),
	ResourceNames = [Name || #xmlElement{name = Name} <- Resources],
	ok = check_resources(ResourceNames).

bulk_cm_epc() ->
	[{userdata, [{doc, "Import bulk CM for epc network resources"}]}].

bulk_cm_epc(Config) ->
	PrivDir = ?config(priv_dir, Config),
	EpcXML = PrivDir ++ "/" ++ "epcNrm.xml",
	ok = cm:import(EpcXML),
	[ConfigData] = decode_xml(xmerl_scan:file(EpcXML)),
	Resources = get_resources(ConfigData#xmlElement.content),
	ResourceNames = [Name || #xmlElement{name = Name} <- Resources],
	ok = check_resources(ResourceNames).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

generate_identity(Length) when Length > 0 ->
	Charset = charset(),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	generate_identity(Random, Charset, NumChars,[]).
%% @hidden
generate_identity(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	generate_identity(Rest, Charset, NumChars, NewAcc);
generate_identity(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

charset() ->
	C1 = lists:seq($A, $D),
	C2 = lists:seq($f, $h),
	C3 = lists:seq($j, $k),
	C4 = lists:seq($m, $n),
	C5 = lists:seq($P, $T),
	C6 = lists:seq($w, $z),
	C7 = lists:seq($ , $ ),
	lists:append([C1, C2, C3, C4, C5, C6, C7]).

decode_xml({#xmlElement{name = _Name, attributes = _Attr, content = Content}, _Rest}) ->
	F = fun(#xmlElement{name = configData}) ->
			true;
		(_) ->
			false
	end,
	lists:filter(F, Content).

get_resources(Content) ->
	F = fun(#xmlElement{}) ->
			true;
		(_) ->
			false
	end,
	[I] = lists:filter(F, Content),
	lists:filter(F, I#xmlElement.content).

check_resources([ResourceName | T]) ->
	case im:get_resource(ResourceName) of
		#resource{name = ResourceName} ->
			check_resources(T);
		{error, Reason} ->
			{error, Reason}
	end;
check_resources([]) ->
	ok.

