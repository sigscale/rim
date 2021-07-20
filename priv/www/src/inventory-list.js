/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import { select } from 'd3-selection';
import { forceSimulation, forceManyBody, forceCenter, forceLink, forceY } from 'd3-force';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-sorter.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class inventoryList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="inventoryGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
					<paper-tabs
							class="details"
							selected="{{selectedTab}}">
						<paper-tab>
							General
						</paper-tab>
						<paper-tab>
							Characteristics
						</paper-tab>
						<paper-tab>
							Relationship
						</paper-tab>
						<paper-tab>
							Connections
						</paper-tab>
						<paper-tab>
							Topology
						</paper-tab>
					</paper-tabs>
					<iron-pages
							id$="tab-[[item.id]]"
							selected="{{selectedTab}}"
						<svg id$="graph-[[item.id]]" />
					<div>
							<dl class="details">
								<template is="dom-if" if="{{item.id}}">
									<dt><b>Id</b></dt>
									<dd>{{item.id}}</dd>
								</template>
								<template is="dom-if" if="{{item.href}}">
									<dt><b>Href</b></dt>
									<dd>{{item.href}}</dd>
								</template>
								<template is="dom-if" if="{{item.publicIdentifier}}">
									<dt><b>Public Id</b></dt>
									<dd>{{item.publicIdentifier}}</dd>
								</template>
								<template is="dom-if" if="{{item.name}}">
									<dt><b>Name</b></dt>
									<dd>{{item.name}}</dd>
								</template>
								<template is="dom-if" if="{{item.description}}">
									<dt><b>Description</b></dt>
									<dd>{{item.description}}</dd>
								</template>
								<template is="dom-if" if="{{item.category}}">
									<dt><b>Category</b></dt>
									<dd>{{item.category}}</dd>
								</template>
								<template is="dom-if" if="{{item.type}}">
									<dt><b>Type</b></dt>
									<dd>{{item.type}}</dd>
								</template>
								<template is="dom-if" if="{{item.schema}}">
									<dt><b>Schema</b></dt>
									<dd>{{item.schema}}</dd>
								</template>
								<template is="dom-if" if="{{item.status}}">
									<dt><b>Status</b></dt>
									<dd>{{item.status}}</dd>
								</template>
								<template is="dom-if" if="{{item.version}}">
									<dt><b>Version</b></dt>
									<dd>{{item.version}}</dd>
								</template>
								<template is="dom-if" if="{{item.start}}">
									<dt><b>Start date</b></dt>
									<dd>{{item.start}}</dd>
								</template>
								<template is="dom-if" if="{{item.end}}">
									<dt><b>End date</b></dt>
									<dd>{{item.end}}</dd>
								</template>
								<template is="dom-if" if="{{item.lastModified}}">
									<dt><b>Last modified</b></dt>
									<dd>{{item.lastModified}}</dd>
								</template>
							</dl>
							<dl class="details">
								<template is="dom-if" if="{{item.resourceSpecification}}">
									<dt><b>Specification</b></dt>
									<dd>{{item.resourceSpecification.id}}</dd>
									<dd>{{item.resourceSpecification.name}}</dd>
									<dd>{{item.resourceSpecification.href}}</dd>
									<dd>{{item.resourceSpecification.version}}</dd>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceChar}}">
								<table class="details">
									<tr>
										<th>Name</th>
										<th>Value</th>
									</tr>
									<template is="dom-repeat" items="{{item.resourceChar}}" as="detail">
										<tr>
											<template is="dom-if" if="{{detail.value}}">
												<td>{{detail.name}}</td>
												<td>{{detail.value}}</td>
											</template>
										</tr>
									</template>
								</table>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceRelationship}}">
								<table class="details">
									<tr>
										<th>Id</th>
										<th>Href</th>
										<th>Name</th>
										<th>Referred Type</th>
										<th>Relationship Type</th>
									</tr>
									<template is="dom-repeat" items="{{item.resourceRelationship}}" as="rel">
										<tr>
											<td>{{rel.id}}</td>
											<td>{{rel.href}}</td>
											<td>{{rel.name}}</td>
											<td>{{rel.referredType}}</td>
											<td>{{rel.relationshipType}}</td>
										</tr>
									</template>
								</table>
							</template>
						</div>
						<div>
							<dl class="details">
								<template is="dom-if" if="{{item.id}}">
									<dt><b>Id</b></dt>
									<dd>{{item.id}}</dd>
								</template>
								<template is="dom-if" if="{{item.href}}">
									<dt><b>Href</b></dt>
									<dd>{{item.href}}</dd>
								</template>
								<template is="dom-if" if="{{item.name}}">
									<dt><b>Name</b></dt>
									<dd>{{item.name}}</dd>
								</template>
								<template is="dom-if" if="{{item.referredType}}">
									<dt><b>ReferredType</b></dt>
									<dd>{{item.referredType}}</dd>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.connectivity}}">
								<svg id$="graph-[[item.id]]" on-click="showFullGraph"/>
							</template>
						</div>
					</iron-pages>
				</template>
				<vaadin-grid-column
						width="40ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filterName"
									aria-label="Name"
									path="name"
									value="{{_filterName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						<div class="timestamp">
							<bdo dir="ltr">[[item.name]]</bdo>
						</div>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="category">
							<vaadin-grid-filter
									id="filterCategory"
									aria-label="Category"
									path="category"
									value="{{_filterCategory}}">
								<input
										slot="filter"
										placeholder="Category"
										value="{{_filterCategory::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						<div>
							[[item.category]]
						</div>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="description">
							<vaadin-grid-filter
									id="filterDesc"
									aria-label="Description"
									path="description"
									value="{{_filterDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						<div>
							[[item.description]]
						</div>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filterType"
									aria-label="Type"
									path="@type"
									value="{{_filterType}}">
								<input
										slot="filter"
										placeholder="Type"
										value="{{_filterType::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						<div>
							[[item.type]]
						</div>
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddInventoryModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="inventoryGetAjax"
				url="resourceInventoryManagement/v4/resource"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			etag: {
				type: String,
				value: null
			},
			activeItem: {
				type: Boolean,
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategory: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterDescription: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterType: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_connections: {
				type: Array
			},
			graphSize: {
				type: Object,
				observer: '_graphSize'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('inventoryGrid');
		grid.dataProvider = this._getInventory;
	}

	connectedCallback() {
		super.connectedCallback();
		this.addEventListener('iron-resize', this.onIronResize);
	}

	disconnectedCallback() {
		super.disconnectedCallback();
		this.removeEventListener('iron-resize', this.onIronResize);
	}

	onIronResize(event) {
		var grid = this.shadowRoot.getElementById('inventoryGrid');
		if (this.activeItem
				&& (event.target.shadowRoot.getElementById('tab-' + this.activeItem.id).selected == 4)
				&& this.activeItem.connectivity
				&& (this.activeItem.connectivity.length > 0)) {
			var connections = this.activeItem.connectivity[0].connection;
			var width = event.target.shadowRoot.getElementById('tab-' + this.activeItem.id).clientWidth;
			var height = Math.ceil(grid.clientHeight / 3);
			var svg = event.target.shadowRoot.getElementById('graph-' + this.activeItem.id);
			svg.setAttribute("height", height);
			var graph = select(svg);
			graph.selectAll('*').remove();
			_connectivityGraph(connections, graph, width, height);
		}
		if (event.path[0].localName == 'iron-pages') {
			grid.notifyResize();
		}
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.inventoryGrid;
			var current;
			if(item == null) {
				current = last;
				this.$.inventoryGrid.selectedItems = item ? [item] : [];
			} else {
				current = item;
				this.$.inventoryGrid.selectedItems = [];
			}
			function checkExist(inventory) {
				return inventory.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	_getInventory(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var inventoryList = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list');
		var ajax = inventoryList.shadowRoot.getElementById('inventoryGetAjax');
		delete ajax.params['filter'];
		var query = "";
		params.filters.forEach(function(filter) {
			if(filter.value) {
				if(filter.value.includes("=")) {
					var sourceReplace = filter.value.replace(/=/g, "\\=").replace(/,/g, "\\,");
					if(query) {
						query = query + "," + filter.path + ".like=[" + sourceReplace + "%]";
					} else {
						query = "[{" + filter.path + ".like=[" + sourceReplace + "%]";
					}
				} else if(query) {
					query = query + "," + filter.path + ".like=[" + filter.value + "%]";
				} else {
					query = "[{" + filter.path + ".like=[" + filter.value + "%]";
				}
			}
		});
		if(query) {
			if(query.includes("like=[%")) {
				delete params.filters[0];
				ajax.params['filter'] = "resourceInventoryManagement/v4/resource";
			} else {
				ajax.params['filter'] = "\"" + query + "}]\"";
			}
		}
		if(inventoryList.etag && params.page > 0) {
			ajax.headers['If-Range'] = inventoryList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				inventoryList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				for(var index in request.response) {
					var newRecord = new Object();
					if(request.response[index].id) {
						newRecord.id = request.response[index].id;
					}
					if(request.response[index].href) {
						newRecord.href = request.response[index].href;
					}
					if(request.response[index].publicIdentifier) {
						newRecord.publicIdentifier = request.response[index].publicIdentifier;
					}
					if(request.response[index].name) {
						newRecord.name = request.response[index].name;
					}
					if(request.response[index].description) {
						newRecord.description = request.response[index].description;
					}
					if(request.response[index].category) {
						newRecord.category = request.response[index].category;
					}
					if(request.response[index]["@type"]) {
						newRecord.type = request.response[index]["@type"];
					}
					if(request.response[index]["@baseType"]) {
						newRecord.base = request.response[index]["@baseType"];
					}
					if(request.response[index]["@schemaLocation"]) {
						newRecord.schema = request.response[index]["@schemaLocation"];
					}
					if(request.response[index].lifecycleStatus) {
						newRecord.status = request.response[index].lifecycleStatus;
					}
					if(request.response[index].lifecycleSubStatus) {
						newRecord.substatus = request.response[index].lifecycleSubStatus;
					}
					if(request.response[index].version) {
						newRecord.version = request.response[index].version;
					}
					if(request.response[index].startDateTime) {
						newRecord.start = request.response[index].startDateTime;
					}
					if(request.response[index].endDateTime) {
						newRecord.end = request.response[index].endDateTime;
					}
					if(request.response[index].lastUpdate) {
						newRecord.lastModified = request.response[index].lastUpdate;
					}
					if(request.response[index].connectivity) {
						newRecord.connectivity = request.response[index].connectivity;
					}
					var resChar = request.response[index].resourceCharacteristic;
					for(var index1 in resChar) {
						if(resChar[index1].value != []) {
							var ValueArray = new Array();
							ValueArray.push(resChar[index1].value);
							for(var str in ValueArray) {
								var str1 = JSON.stringify(ValueArray[str]);
								var str2 = str1.trim();
								var res = str2.replace(/"|{|[|[|}|]|]/g, " ");
								resChar[index1].value = res;
								newRecord.resourceChar = resChar;
							}
						} else {
							newRecord.resourceChar = resChar;
						}
					}
					if(request.response[index].resourceRelationship) {
						var relArray = new Array();
						for(var indexRel in request.response[index].resourceRelationship) {
							var relObj = new Object();
							relObj.id = request.response[index].resourceRelationship[indexRel].resource.id
							relObj.name = request.response[index].resourceRelationship[indexRel].resource.name
							relObj.href = request.response[index].resourceRelationship[indexRel].resource.href;
							relObj.referredType = request.response[index].resourceRelationship[indexRel].resource["@referredType"];
							relObj.relationshipType = request.response[index].resourceRelationship[indexRel].relationshipType;
							relArray.push(relObj);
						}
						newRecord.resourceRelationship = relArray;
					}
					if(request.response[index].connectionPoint) {
						for(var indexCon in request.response[index].connectionPoint) {
							newRecord.referredType = request.response[index].connectionPoint[indexCon]["@referredType"];
							newRecord.href = request.response[index].connectionPoint[indexCon].href;
							newRecord.id = request.response[index].connectionPoint[indexCon].id;
							newRecord.name = request.response[index].connectionPoint[indexCon].name;
						}
					}
					if(request.response[index].resourceSpecification) {
						newRecord.resourceSpecification = request.response[index].resourceSpecification;
					}
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			inventoryList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (inventoryList.etag && params.page > 0) {
					ajax.headers['If-Range'] = inventoryList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (inventoryList.etag && params.page > 0) {
				ajax.headers['If-Range'] = inventoryList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('inventoryGrid');
		grid.size = 0;
	}

	showAddInventoryModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-add').shadowRoot.getElementById('inventoryAddModal').open();
	}

	showFullGraph(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-topology').shadowRoot.getElementById('topologyGraph').open();
	}

	_graphSize() {
		var topologyGraph = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-topology').shadowRoot.getElementById('topologyGraph');
		var graph = select(topologyGraph).select('#graph');
		graph.selectAll('*').remove();
		if ((this.graphSize.width > 0) && (this.graphSize.height > 0)) {
			var svg = topologyGraph.querySelector('#graph');
			var connections = this.activeItem.connectivity[0].connection;
			_connectivityGraph(connections, graph, svg.clientWidth, svg.clientHeight);
		}
	}
}

function _connectivityGraph(connections, graph, width, height) {
	var vertices = [];
	function mapEdge(connection) {
		let edge = {};
		if(connection.id) {
			edge.id = connection.id;
		}
		if(connection.name) {
			edge.name = connection.name;
		}
		if(connection.associationType) {
			edge.associationType = connection.associationType;
		}
		if(connection.endpoint) {
			let index = vertices.findIndex(function (vertex) {
				return vertex.id == connection.endpoint[0].id;
			});
			if(index == -1) {
				let v0 = {};
				v0.id = connection.endpoint[0].id;
				if(connection.endpoint[0].name) {
					v0.name = connection.endpoint[0].name;
				}
				if(connection.endpoint[0]["@referredType"]) {
					v0.type = connection.endpoint[0]["@referredType"];
				}
				edge.source = vertices.push(v0) - 1;
			} else {
				edge.source = index;
			}
			index = vertices.findIndex(function (vertex) {
				return vertex.id == connection.endpoint[1].id;
			});
			if(index == -1) {
				let v1 = {};
				v1.id = connection.endpoint[1].id;
				if(connection.endpoint[1].name) {
					v1.name = connection.endpoint[1].name;
				}
				if(connection.endpoint[1]["@referredType"]) {
					v1.type = connection.endpoint[1]["@referredType"];
				}
				edge.target = vertices.push(v1) - 1;
			} else {
				edge.target = index;
			}
		}
		return edge;
	}
	var edges = connections.map(mapEdge);
	var simulation = forceSimulation(vertices)
			.force("center", forceCenter(Math.ceil(width/2), Math.ceil(height/2)))
			.force("charge", forceManyBody().strength(-800))
			.force("link", forceLink(edges).distance(100))
			.force('y', forceY());
	var edge = graph.selectAll('.edge')
			.data(edges)
			.enter()
			.append('line')
					.attr('class', 'edge');
	var vertex = graph.selectAll('g.vertex')
			.data(vertices);
	var vgroup = vertex.enter()
			.append('g')
	var circle = vgroup.append('circle')
			.attr('r', Math.ceil(width / 100))
			.attr('class', 'vertex')
			.append('title')
			.text(function(d) { return d.name});
	vgroup.append('text')
			.text(function(d) { return d.name})
			.attr('y', - Math.ceil(width / 100) - 8 )
			.attr('text-anchor', 'middle');
	simulation.on('tick', function() {
		vgroup.attr('transform', function(d) {
				return 'translate(' + Math.ceil(d.x) + ',' + Math.ceil(d.y) + ')';
		});
		edge.attr('x1', function(d) { return Math.ceil(d.source.x) })
				.attr('y1', function(d) { return Math.ceil(d.source.y) })
				.attr('x2', function(d) { return Math.ceil(d.target.x) })
				.attr('y2', function(d) { return Math.ceil(d.target.y) });
	});
}

window.customElements.define('inventory-list', inventoryList);
