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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import './style-element.js';

class specificationList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="specificationGrid">
				<vaadin-grid-column>
					<template class="header">
						Name
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Description
					</template>
					<template>
						[[item.description]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Class
					</template>
					<template>
						[[item.classs]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Status
					</template>
					<template>
						[[item.stat]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Category	
					</template>
					<template>
						[[item.cat]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Bundle
					</template>
					<template>
						[[item.bundle]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax
				id="getSpecificationAjax"
				url="resourceCatalogManagement/v3/specification1"
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
		var grid = this.shadowRoot.getElementById('specificationGrid');
		var ajaxGrid = this.shadowRoot.getElementById('getSpecificationAjax');
		grid.dataProvider = this._getSpecification;
	}

	_getSpecification(params, callback) {
		var grid = this;
		var specificationList = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list').shadowRoot.getElementById('getSpecificationAjax');
		if(specificationList.etag && params.page > 0) {
			headers['If-Range'] = specificationList.etag;
		}
		var specificationList1 = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				specificationList1.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				callback(vaadinItems);
		} else {
			grid.size = 0;
			callback([]);
		}
	};
	var handleAjaxError = function(error) {
		specificationList1.etag = null;
		var toast;
		toast.text = "error";
		toast.open();
		if(!grid.size) {
			grid.size = 0;
		}
		callback([]);
	}
	if(specificationList.loading) {
		specificationList.lastRequest.completes.then(function(request) {
			var startRange = params.page * params.pageSize + 1;
			specificationList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (specificationList1.etag && params.page > 0) {
					specificationList.headers['If-Range'] = userList1.etag;
				} else {
					delete specificationList.headers['If-Range'];
				}
				return specificationList.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
			} else {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				specificationList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (specificationList1.etag && params.page > 0) {
					specificationList.headers['If-Range'] = userList1.etag;
				} else {
					delete specificationList.headers['If-Range'];
				}
			specificationList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
} 

window.customElements.define('specification-list', specificationList);
