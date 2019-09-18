/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/paper-toast/paper-toast.js';
import '@polymer/iron-icons/iron-icons.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-sorter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js';

class inventoryList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="inventoryGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
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
							<dt><b>Class</b></dt>
							<dd>{{item.type}}</dd>
						</template>
						<template is="dom-if" if="{{item.base}}">
							<dt><b>Type</b></dt>
							<dd>{{item.base}}</dd>
						</template>
						<template is="dom-if" if="{{item.schema}}">
							<dt><b>Schema</b></dt>
							<dd>{{item.schema}}</dd>
						</template>
						<template is="dom-if" if="{{item.lifecycleState}}">
							<dt><b>Status</b></dt>
							<dd>{{item.lifecycleState}}</dd>
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
					<h3 class="inventoryH3">Resource Characteristics:</h3>
					<dl class="details">
						<template is="dom-if" if="{{item.resourceChar}}">
							<template is="dom-repeat" items="{{item.resourceChar}}" as="detail">
								<dt>{{detail.name}}</dt>
								<dd>{{detail.value}}</dd>
							</template>
						</template>
					</dl>
				</template>
				<vaadin-grid-column width="13ex" flex-grow="2">
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
				<vaadin-grid-column width="11ex" flex-grow="2">
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
				<vaadin-grid-column width="11ex" flex-grow="2">
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
				<vaadin-grid-column width="11ex" flex-grow="2">
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
         <paper-toast
            id="inventoryError">
         </paper-toast>
			<iron-ajax
				id="getInventoryAjax"
				url="resourceInventoryManagement/v3/resource"
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
			}
		}
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.shadowRoot.getElementById('inventoryGrid');
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item
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

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('inventoryGrid');
		grid.dataProvider = this._getInventory;
	}

	_getInventory(params, callback) {
		var grid = this;
		var ajax = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list').shadowRoot.getElementById('getInventoryAjax');
		var inventoryList = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list');
		delete ajax.params['filter'];
		delete ajax.params['sort'];
		var query = "";
		function checkLike(param) {
			return param.path == "name" || param.path == "description"
				|| param.path == "@type" || param.path == "category"
				|| param.path == "lifecycleState" || param.path == "lifecycleSubState";
		}
		params.filters.filter(checkLike).forEach(function(filter) {
			if(filter.value) {
				if(filter.path == "name") {
					if(filter.value.includes("=")) {
						var sourceReplace = filter.value.replace(/=/g, "\\=");
						if(query) {
							query = query + "," + filter.path + ".like=[" + sourceReplace + "%]";
						} else {
							query = "[{" + filter.path + ".like=[" + sourceReplace + "%]";
						}
						if(sourceReplace.includes(",")) {
							var sourceReplace1 = sourceReplace.replace(/,/g, "\\,");
							if(query) {
								query = query + "," + filter.path + ".like=[" + sourceReplace1 + "%]";
							} else {
								query = "[{" + filter.path + ".like=[" + sourceReplace1 + "%]";
							}
						}
					} else if(query) {
						query = query + "," + filter.path + ".like=[" + filter.value + "%]";
					} else {
						query = "[{" + filter.path + ".like=[" + filter.value + "%]";
					}
				} else if (query) {
					query = query + "," + filter.path + ".like=[" + filter.value + "%]";
				} else {
					query = "[{" + filter.path + ".like=[" + filter.value + "%]";
				}
			}
		});
		if(query) {
			if(query.includes("like=[%")) {
				delete params.filters[0];
				ajax.params['filter'] = "resourceInventoryManagement/v3/resource";
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
					if(request.response[index].lifecycleState) {
						newRecord.lifecycleState = request.response[index].lifecycleState;
					}
					if(request.response[index].lifecycleSubState) {
						newRecord.lifecycleSubState = request.response[index].lifecycleSubState;
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
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			inventoryList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list').shadowRoot.getElementById('inventoryError');
			toast.text = error;
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
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
		document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-add').shadowRoot.getElementById('addInventoryModal').open();
	}
}

window.customElements.define('inventory-list', inventoryList);
