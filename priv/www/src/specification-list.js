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

class specificationList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="specificationGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
					<paper-tabs selected="{{selectedTab}}">
						<paper-tab>
							General
						</paper-tab>
						<paper-tab>
							Characteristics
						</paper-tab>
						<paper-tab>
							Features
						</paper-tab>
						<paper-tab>
							Relationships
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
							selected="{{selectedTab}}">
						<div>
							<dl class="details">
								<template is="dom-if" if="{{item.id}}">
									<dt><b>Id</b></dt>
									<dd>{{item.id}}</dd>
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
								<template is="dom-if" if="{{item.isBundle}}">
									<dt><b>Isbundle</b></dt>
									<dd>{{item.isBundle}}</dd>
								</template>
								<template is="dom-if" if="{{item.lifecycleStatus}}">
									<dt><b>Status</b></dt>
									<dd>{{item.lifecycleStatus}}</dd>
								</template>
								<template is="dom-if" if="{{item.lastUpdate}}">
									<dt><b>Last Update</b></dt>
									<dd>{{item.lastUpdate}}</dd>
								</template>
								<template is="dom-if" if="{{item.version}}">
									<dt><b>Version</b></dt>
									<dd>{{item.version}}</dd>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceSpecCharacteristic}}">
								<dl class="details">
									<template is="dom-repeat" items="{{item.resourceSpecCharacteristic}}" as="char">
										<template is="dom-if" if="{{char.id}}">
											<dt>id</dt>
											<dd>{{char.id}}</dd>
										</template>
										<template is="dom-if" if="{{char.name}}">
											<dt>name</dt>
											<dd>{{char.name}}</dd>
										</template>
										<template is="dom-if" if="{{char.description}}">
											<dt>description</dt>
											<dd>{{char.description}}</dd>
										</template>
										<template is="dom-if" if="{{char.configurable}}">
											<dt>configurable</dt>
											<dd>{{char.configurable}}</dd>
										</template>
										<template is="dom-if" if="{{char.extensible}}">
											<dt>extensible</dt>
											<dd>{{char.extensible}}</dd>
										</template>
										<template is="dom-if" if="{{char.isUnique}}">
											<dt>isUnique</dt>
											<dd>{{char.isUnique}}</dd>
										</template>
										<template is="dom-if" if="{{char.maxCardinality}}">
											<dt>maxCardinality</dt>
											<dd>{{char.maxCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{char.minCardinality}}">
											<dt>minCardinality</dt>
											<dd>{{char.minCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{char.regex}}">
											<dt>regex</dt>
											<dd>{{char.regex}}</dd>
										</template>
										<template is="dom-if" if="{{char.valueType}}">
											<dt>valueType</dt>
											<dd>{{char.valueType}}</dd>
										</template>
									</template>
								</dl>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.featureSpecCharacteristic}}">
								<dl class="details">
									<template is="dom-repeat" items="{{item.featureSpecCharacteristic}}" as="feat">
										<template is="dom-if" if="{{feat.name}}">
											<dt>name</dt>
											<dd>{{feat.name}}</dd>
										</template>
										<template is="dom-if" if="{{feat.description}}">
											<dt>description</dt>
											<dd>{{feat.description}}</dd>
										</template>
										<template is="dom-if" if="{{feat.configurable}}">
											<dt>configurable</dt>
											<dd>{{feat.configurable}}</dd>
										</template>
										<template is="dom-if" if="{{feat.extensible}}">
											<dt>extensible</dt>
											<dd>{{feat.extensible}}</dd>
										</template>
										<template is="dom-if" if="{{feat.isUnique}}">
											<dt>isUnique</dt>
											<dd>{{feat.isUnique}}</dd>
										</template>
										<template is="dom-if" if="{{feat.maxCardinality}}">
											<dt>maxCardinality</dt>
											<dd>{{feat.maxCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{feat.minCardinality}}">
											<dt>minCardinality</dt>
											<dd>{{feat.minCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{feat.regex}}">
											<dt>regex</dt>
											<dd>{{feat.regex}}</dd>
										</template>
										<template is="dom-if" if="{{feat.valueType}}">
											<dt>valueType</dt>
											<dd>{{feat.valueType}}</dd>
										</template>
									</template>
								</dl>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceSpecRelationship}}">
								<dl class="details">
									<template is="dom-repeat" items="{{item.resourceSpecRelationship}}" as="rel">
										<template is="dom-if" if="{{rel.id}}">
											<dt>id</dt>
											<dd>{{rel.id}}</dd>
										</template>
										<template is="dom-if" if="{{rel.href}}">
											<dt>href</dt>
											<dd>{{rel.href}}</dd>
										</template>
										<template is="dom-if" if="{{rel.name}}">
											<dt>name</dt>
											<dd>{{rel.name}}</dd>
										</template>
										<template is="dom-if" if="{{rel.description}}">
											<dt>description</dt>
											<dd>{{rel.description}}</dd>
										</template>
										<template is="dom-if" if="{{rel.defaultQuantity}}">
											<dt>defaultQuantity</dt>
											<dd>{{rel.defaultQuantity}}</dd>
										</template>
										<template is="dom-if" if="{{rel.maxCardinality}}">
											<dt>maxCardinality</dt>
											<dd>{{rel.maxCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{rel.minCardinality}}">
											<dt>minCardinality</dt>
											<dd>{{rel.minCardinality}}</dd>
										</template>
										<template is="dom-if" if="{{rel.role}}">
											<dt>role</dt>
											<dd>{{rel.role}}</dd>
										</template>
										<template is="dom-if" if="{{rel.relationshipType}}">
											<dt>relationshipType</dt>
											<dd>{{rel.relationshipType}}</dd>
										</template>
									</template>
								</dl>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.connectionPointSpecification}}">
								<dl class="details">
									<template is="dom-repeat" items="{{item.connectionPointSpecification}}" as="cp">
										<template is="dom-if" if="{{cp.id}}">
											<dt>id</dt>
											<dd>{{cp.id}}</dd>
										</template>
										<template is="dom-if" if="{{cp.href}}">
											<dt>href</dt>
											<dd>{{cp.href}}</dd>
										</template>
										<template is="dom-if" if="{{cp.name}}">
											<dt>name</dt>
											<dd>{{cp.name}}</dd>
										</template>
										<template is="dom-if" if="{{cp.version}}">
											<dt>version</dt>
											<dd>{{cp.version}}</dd>
										</template>
										<template is="dom-if" if="{{_formatName(cp, '@referredType')}}">
											<dt>@referredType</dt>
											<dd>{{_formatName(cp, '@referredType')}}</dd>
										</template>
									</template>
								</dl>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.connectivitySpecification}}">
								<svg id$="graph-[[item.id]]" />
							</template>
						</div>
					</iron-pages>
				</template>
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filterSpecName"
									aria-label="Name"
									path="name"
									value="{{_filterSpecName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterSpecName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="20ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-sorter
								path="decription">
							<vaadin-grid-filter
									id="filterSpecDesc"
									aria-label="Description"
									path="description"
									value="{{_filterSpecDesc}}">
								<input
									slot="filter"
									placeholder="Description"
									value="{{_filterSpecDesc::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filterSpecClass"
									aria-label="Class"
									path="@type"
									value="{{_filterSpecClass}}">
								<input
									slot="filter"
									placeholder="Class"
									value="{{_filterSpecClass::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.type]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="lifecycleStatus">
							<vaadin-grid-filter
									id="filterSpecStatus"
									aria-label="Status"
									path="lifecycleStatus"
									value="{{_filterSpecStatus}}">
								<input
									slot="filter"
									placeholder="Status"
									value="{{_filterSpecStatus::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.status]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="category">
							<vaadin-grid-filter
									id="filterSpecCat"
									aria-label="Category"
									path="category"
									value="{{_filterSpecCategory}}">
								<input
									slot="filter"
									placeholder="Category"
									value="{{_filterSpecCategory::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.category]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="isBundle">
							<vaadin-grid-filter
									id="filterSpecBundle"
									aria-label="Bundle"
									path="isBundle"
									value="{{_filterSpecBundle}}">
								<input
									slot="filter"
									placeholder="Bundle"
									value="{{_filterSpecBundle::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.bundle]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="_showAddSpecificationModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="specificationGetAjax"
				url="resourceCatalogManagement/v4/resourceSpecification"
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
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterSpecName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecDesc: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecClass: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecCategory: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecBundle: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
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
		if (this.activeItem
				&& (event.target.shadowRoot.getElementById('tab-' + this.activeItem.id).selected == 5)
				&& this.activeItem.connectivitySpecification
				&& (this.activeItem.connectivitySpecification.length > 0)) {
			var connectivity = this.activeItem.connectivitySpecification.shift().connectionSpecification;
			var width = event.target.shadowRoot.getElementById('tab-' + this.activeItem.id).clientWidth;
			var height = event.target.shadowRoot.getElementById('tab-' + this.activeItem.id).clientHeight;
			var svg = event.target.shadowRoot.getElementById('graph-' + this.activeItem.id);
			svg.setAttribute("viewBox", "0 0 " + width + " " + height);
			var graph = select(svg);
			graph.selectAll('*').remove();
			_connectivityGraph(connectivity, graph, width, height);
		}
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.specificationGrid;
			var current;
			if(item == null) {
				current = last;
				this.$.specificationGrid.selectedItems = item ? [item] : [];
			} else {
				current = item;
				this.$.specificationGrid.selectedItems = [];
			}
			function checkExist(specification) {
				return specification.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.dataProvider = this._getSpecification;
	}

	_formatName(obj, prop) {
		return obj[prop];
	}

	_getSpecification(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var specificationList = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list');
		var ajax = specificationList.shadowRoot.getElementById('specificationGetAjax');
		delete ajax.params['filter'];
		var query = "";
		params.filters.forEach(function(filter) {
			if(filter.path != "isBundle") {
				if(filter.value) {
					if (query) {
						query = query + "," + filter.path + ".like=[" + filter.value + "%]";
					} else {
						query = "[{" + filter.path + ".like=[" + filter.value + "%]";
					}
				}
			} else if(filter.path == "lifecycleStatus") {
				if(filter.value) {
					if("Obsolete".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Obsolete";
						} else {
							query = "[{lifecycleStatus=Obsolete";
						}
					} else if("Launched".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Launched";
						} else {
							query = "[{lifecycleStatus=Launched";
						}
					} else if("Active".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Active";
						} else {
							query = "[{lifecycleStatus=Active";
						}
					} else if("In ".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus.in=[In Study,In Design, In Test]";
						} else {
							query = "[{lifecycleStatus.in=[In Study,In Design, In Test]";
						}
					} else if("In Study".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=In Study";
						} else {
							query = "[{lifecycleStatus=In Study";
						}
					} else if("In Design".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=In Design";
						} else {
							query = "[{lifecycleStatus=In Design";
						}
					} else if("Re".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus.in=[Rejected,Retired]";
						} else {
							query = "[{lifecycleStatus.in=[Rejected,Retired]";
						}
					} else if("Rejected".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Rejected";
						} else {
							query = "[{lifecycleStatus=Rejected";
						}
					} else if("Retired".startsWith(filter.value)) {
						if (query) {
							query = query + ",lifecycleStatus=Retired";
						} else {
							query = "[{lifecycleStatus=Retired";
						}
					} else {
						if (query) {
							query = query + ",lifecycleStatus=" + filter.value;
						} else {
							query = "[{lifecycleStatus=" + filter.value;
						}
					}
				}
			} else {
				if(filter.value) {
					if("true".startsWith(filter.value)) {
						if (query) {
							query = query + ",isBundle=true";
						} else {
							query = "[{isBundle=true";
						}
					} else if("false".startsWith(filter.value)) {
						if (query) {
							query = query + ",isBundle=false";
						} else {
							query = "[{isBundle=false";
						}
					} else {
						if (query) {
							query = query + ",isBundle=" + filter.value;
						} else {
							query = "[{isBundle=" + filter.value;
						}
					}
				}
			}
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
		}
		if(specificationList.etag && params.page > 0) {
			ajax.headers['If-Range'] = specificationList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				specificationList.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.id = request.response[index].id;
					newRecord.name = request.response[index].name;
					newRecord.description = request.response[index].description;
					newRecord.type = request.response[index]["@type"];
					newRecord.base = request.response[index]["@baseType"];
					newRecord.status = request.response[index].lifecycleStatus;
					newRecord.category = request.response[index].category;
					newRecord.bundle = request.response[index].isBundle;
					newRecord.lastUpdate = request.response[index].lastUpdate;
					newRecord.version = request.response[index].version;
					newRecord.feature = request.response[index].resourceSpecFeature;
					newRecord.resourceSpecCharacteristic = request.response[index].resourceSpecCharacteristic;
					newRecord.resourceSpecRelationship= request.response[index].resourceSpecRelationship;
					newRecord.connectionPointSpecification = request.response[index].connectionPointSpecification;
					if(request.response[index].connectivitySpecification) {
						newRecord.connectivitySpecification = request.response[index].connectivitySpecification;
					}
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			specificationList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (specificationList.etag && params.page > 0) {
						ajax.headers['If-Range'] = specificationList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (specificationList.etag && params.page > 0) {
				ajax.headers['If-Range'] = specificationList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.size = 0;
	}

	_showupdateSpecificationModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-update').shadowRoot.getElementById('specificationUpdateModal').open();
	}

	_showAddSpecificationModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-add').shadowRoot.getElementById('specificationAddModal').open();
	}

}

function _connectivityGraph(connectivity, graph, width, height) {
	var vertices = [];
	function mapEdge(connectivitySpecification) {
		let edge = {};
		if(connectivitySpecification.name) {
			edge.name = connectivitySpecification.name;
		}
		if(connectivitySpecification.endpointSpecification) {
			let index1 = vertices.findIndex(function (vertex1) {
				return vertex1.id == connectivitySpecification.endpointSpecification[0].id;
			});
			if(index1 == -1) {
				let v1 = {};
				v1.id = connectivitySpecification.endpointSpecification[0].id;
				if(connectivitySpecification.endpointSpecification[0].href) {
					v1.href = connectivitySpecification.endpointSpecification[0].href;
				}
				if(connectivitySpecification.endpointSpecification[0]["@referredType"]) {
					v1.type = connectivitySpecification.endpointSpecification[0]["@referredType"];
				}
				if(connectivitySpecification.endpointSpecification[0].name) {
					v1.name = connectivitySpecification.endpointSpecification[0].name;
				}
				edge.source = vertices.push(v1) - 1;
			} else {
				edge.source = index1;
			}
			index1 = vertices.findIndex(function (vertex1) {
				return vertex1.id == connectivitySpecification.endpointSpecification[1].id;
			});
			 if(index1 == -1) {
				let v2 = {};
				v2.id = connectivitySpecification.endpointSpecification[1].id;
				if(connectivitySpecification.endpointSpecification[1].href) {
					v2.href = connectivitySpecification.endpointSpecification[1].href;
				}
				if(connectivitySpecification.endpointSpecification[1]["@referredType"]) {
					v2.type = connectivitySpecification.endpointSpecification[1]["@referredType"];
				}
				if(connectivitySpecification.endpointSpecification[1].name) {
					v2.name = connectivitySpecification.endpointSpecification[1].name;
				}
				if(connectivitySpecification.endpointSpecification[1].connectionPointSpecification) {
					if(connectivitySpecification.endpointSpecification[1].connectionPointSpecification.id) {
						v2.pointId = connectivitySpecification.endpointSpecification[1].connectionPointSpecification.id;
					}
					if(connectivitySpecification.endpointSpecification[1].connectionPointSpecification.href) {
						v2.pointHref = connectivitySpecification.endpointSpecification[1].connectionPointSpecification.href;
					}
					if(connectivitySpecification.endpointSpecification[1].connectionPointSpecification["@referredType"]) {
						v2.pointType = connectivitySpecification.endpointSpecification[1].connectionPointSpecification["@referredType"];
					}
					if(connectivitySpecification.endpointSpecification[1].connectionPointSpecification.name) {
						v2.pointName = connectivitySpecification.endpointSpecification[1].connectionPointSpecification.name;
					}
				}
				edge.target = vertices.push(v2) - 1;
			} else {
				edge.target = index1;
			}
		}
		return edge;
	}
	var edges = connectivity.map(mapEdge);
	var simulation = forceSimulation(vertices)
			.force("center", forceCenter(Math.ceil(width/2), Math.ceil(height/2)))
			.force("charge", forceManyBody().strength(-800))
			.force("link", forceLink(edges).distance(100))
			.force('y', forceY());
	var edge1 = graph.selectAll('.edge')
			.data(edges)
			.enter()
			.append('line')
					.attr('class', 'edge');
	var vertex1 = graph.selectAll('g.vertex1')
			.data(vertices);
	var vgroup = vertex1.enter()
		.append('g')
	var circle = vgroup.append('circle')
			.attr('r', Math.ceil(width / 100))
			.attr('class', 'vertex')
			.append('title')
			.text(function(d) { return d.name});
	vgroup.append('text')
			.text(function(d) { return d.type })
			.attr('y', - Math.ceil(width / 100) - 8 )
			.attr('text-anchor', 'middle');
	simulation.on('tick', function() {
		vgroup.attr('transform', function(d) {
			return 'translate(' + Math.ceil(d.x) + ',' + Math.ceil(d.y) + ')';
		});
		edge1.attr('x1', function(d) { return Math.ceil(d.source.x) })
				.attr('y1', function(d) { return Math.ceil(d.source.y) })
				.attr('x2', function(d) { return Math.ceil(d.target.x) })
				.attr('y2', function(d) { return Math.ceil(d.target.y) });
	});
}

window.customElements.define('specification-list', specificationList);
