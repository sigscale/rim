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
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
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
						<dt><b>Inventory Details:</b></dt>
						<dd>{{item.inventoryDetails}}</dd>
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
		grid.dataProvider = this._getInventoryList;
	}

	_getInventoryList(params, callback) {
		var grid = this;
		var ajax = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list').shadowRoot.getElementById('getInventoryAjax');
		var inventoryList = document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-list');
		delete ajax.params['filter'];
		delete ajax.params['sort'];
		var query = "";
		function checkLike(param) {
			return param.path == "name" || param.path == "description"
				|| param.path == "@type" || param.path == "category";
		}
		params.filters.filter(checkLike).forEach(function(filter) {
			if(filter.value) {
				if (query) {
					query = query + "," + filter.path + ".like=[" + filter.value + "%]";
				} else {
					query = "[{" + filter.path + ".like=[" + filter.value + "%]";
				}
			}
		});
		function checkLifeCycle(param) {
			return param.path == "lifecycleStatus";
		}
		params.filters.filter(checkLifeCycle).forEach(function(filter) {
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
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
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
					newRecord.id = request.response[index].id;
					newRecord.name = request.response[index].name;
					newRecord.category = request.response[index].category;
					newRecord.description = request.response[index].description;
					newRecord.type = request.response[index]["@type"];
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
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(ajax.loading) {
console.log(ajax.loading);
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
console.log(ajax.loading);
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
}

window.customElements.define('inventory-list', inventoryList);
