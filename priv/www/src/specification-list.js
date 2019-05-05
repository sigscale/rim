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
import './style-element.js';

class specificationListAjax extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="specificationGrid"
					active-item="{{activeItem}}">
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-sorter
								path="specName">
							<vaadin-grid-filter
									id="filterSpecName"
									aria-label="Name"
									path="specName"
									value="{{_filterSpecName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterSpecName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.specName]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="20ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-sorter
								path="specDesc">
							<vaadin-grid-filter
									id="filterSpecDesc"
									aria-label="Description"
									path="specDesc"
									value="{{_filterSpecDesc}}">
								<input
									slot="filter"
									placeholder="Description"
									value="{{_filterSpecDesc::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.specDesc]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="specClass">
							<vaadin-grid-filter
									id="filterSpecClass"
									aria-label="Class"
									path="specClass"
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
						[[item.specClass]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="specStatus">
							<vaadin-grid-filter
									id="filterSpecStatus"
									aria-label="Status"
									path="specStatus"
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
						[[item.specStatus]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="specCat">
							<vaadin-grid-filter
									id="filterSpecCat"
									aria-label="Category"
									path="specCat"
									value="{{_filterSpecCat}}">
								<input
									slot="filter"
									placeholder="Category"
									value="{{_filterSpecCat::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.specCat]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="specBundle">
							<vaadin-grid-filter
									id="filterSpecBundle"
									aria-label="Bundle"
									path="specBundle"
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
						[[item.specBundle]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddSpecificationModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getSpecificationAjax"
				url="resourceCatalogManagement/v3/resourceSpecification"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			etag: {
				type: String,
				value: null
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			}

		}
	}

	_activeItemChanged(item) {
		if(item) {
			var grid = this.$.specificationGrid;
			grid.selectedItems = item ? [item] : [];
			var updateSpec = document.querySelector('inventory-management').shadowRoot.getElementById('updateSpec');
			updateSpec.shadowRoot.getElementById('updateSpecModal').open();
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.dataProvider = this._getSpecification;
	}

	_getSpecification(params, callback) {
		var grid = this;
		var specificationListAjax = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list').shadowRoot.getElementById('getSpecificationAjax');
		var query = "";
		function checkHead(param) {
			return param.path == "specName" || param.path == "specDesc"
				|| param.path == "specClass" || param.path == "specStatus"
				|| param.path == "specCat" || param.path == "specBundle";
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if(filter.value) {
				if (query) {
					query = query + "]," + filter.path + ".like=[" + filter.value + "%";
				} else {
					query = "[{" + filter.path + ".like=[" + filter.value + "%";
				}
			}
		});
		if(query) {
			if(query.includes("like=[%")) {
				delete params.filters[0];
				specificationListAjax.params['filter'] = "resourceCatalogManagement/v3/specification";
			} else {
				specificationListAjax.params['filter'] = "\"" + query + "]}]\"";
			}
		}
		if(specificationListAjax.etag && params.page > 0) {
			headers['If-Range'] = specificationListAjax.etag;
		}
		var specificationList = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list');
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
					newRecord.specName = request.response[index].name;
					newRecord.specDesc = request.response[index].description;
					newRecord.specClass = request.response[index]["@type"];
					newRecord.specStatus = request.response[index].lifecycleStatus;
					newRecord.specCat = request.response[index].category;
					newRecord.specBundle = request.response[index].isBundle;
					newRecord.specChars = request.response[index].resourceSpecCharacteristic;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
		} else {
			grid.size = 0;
			callback([]);
		}
	};
	var handleAjaxError = function(error) {
		specificationList.etag = null;
		var toast;
		toast.text = "error";
		toast.open();
		if(!grid.size) {
			grid.size = 0;
		}
		callback([]);
	}
	if(specificationListAjax.loading) {
		specificationListAjax.lastRequest.completes.then(function(request) {
			var startRange = params.page * params.pageSize + 1;
			specificationListAjax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (specificationList.etag && params.page > 0) {
					specificationListAjax.headers['If-Range'] = userList1.etag;
				} else {
					delete specificationListAjax.headers['If-Range'];
				}
				return specificationListAjax.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
			} else {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				specificationListAjax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (specificationList.etag && params.page > 0) {
					specificationListAjax.headers['If-Range'] = userList1.etag;
				} else {
					delete specificationListAjax.headers['If-Range'];
				}
			specificationListAjax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
} 

window.customElements.define('specification-list', specificationListAjax);
