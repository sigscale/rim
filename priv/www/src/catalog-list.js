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

class catalogListAjax extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="catalogGrid">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="catalogName">
							<vaadin-grid-filter
									id="filterCatalogName"
									aria-label="Name"
									path="catalogName"
									value="{{_filterCatalogName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterCatalogName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.catalogName]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="catalogDescription">
							<vaadin-grid-filter
									id="filterCatalogDescription"
									aria-label="Description"
									path="catalogDescription"
									value="{{_filterCatalogDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterCatalogDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.catalogDescription]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="catalogClass">
							<vaadin-grid-filter
									id="filterCatalogClass"
									aria-label="Class"
									path="catalogClass"
									value="{{_filterCatalogClass}}">
								<input
										slot="Filter"
										placeholder="Class
										value="{{_filterCatalogClass::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.catalogClass]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="catalogStatus">
							<vaadin-grid-filter
									id="filterCatalogStatus"
									aria-label="Status"
									path="catalogStatus"
									value="{{_filterCatalogStatus}}">
								<input
										slot="Filter"
										placeholder="Status"
										value="{{_filterCatalogStatus::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.catalogStatus]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddCatalogModal">
				</paper-fab>
			</div>
			<paper-toast
					id="catalogError">
			</paper-toast>
			<iron-ajax
				id="getCatalogAjax"
				url="resourceCatalogManagement/v3/resourceCatalog"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('catalogGrid');
		var ajaxGrid = this.shadowRoot.getElementById('getCatalogAjax');
		grid.dataProvider = this._getCatalog;
	}

	_getCatalog(params, callback) {
		var grid = this;
		var catalogListAjax = document.body.querySelector('inventory-management').shadowRoot.querySelector('catalog-list').shadowRoot.getElementById('getCatalogAjax');
		var query = "";
		function checkHead(param) {
			return param.path == "catalogName" || param.path == "catalogDescription"
					|| param.path == "catalogClass" || param.path == "catalogStatus";
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
				catalogList.params['filter'] = "resourceCatalogManagement/v3/catalog";
			} else {
				catalogList.params['filter'] = "\"" + query + "]}]\"";
			}
		}
		if(catalogList.etag && params.page > 0) {
			headers['If-Range'] = catalogList.etag;
		}
		var catalogList = document.body.querySelector('inventory-management').shadowRoot.querySelector('catalog-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				catalogList1.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.catalogName = request.response[index].name;
					newRecord.catalogDescription = request.response[index].description;
					newRecord.catalogClass = request.response[index]["@type"];
					newRecord.catalogStatus = request.response[index].lifecycleStatus;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			catalogList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.querySelector('catalog-list').shadowRoot.getElementById('catalogError');
			toast.text = error;
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(catalogList.loading) {
			catalogList.lastRequest.completes.then(function(request) {
					var startRange = params.page * params.pageSize + 1;
					catalogList.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (catalogList1.etag && params.page > 0) {
						catalogList.headers['If-Range'] = userList1.etag;
					} else {
						delete catalogList.headers['If-Range'];
					}
					return catalogList.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			catalogList.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (catalogList1.etag && params.page > 0) {
				catalogList.headers['If-Range'] = userList1.etag;
			} else {
				delete catalogList.headers['If-Range'];
			}
			catalogList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
} 

window.customElements.define('catalog-list', catalogListAjax);
