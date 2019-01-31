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

class candidateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="candidateGrid">
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
			</vaadin-grid>
			<iron-ajax
				id="getCandidateAjax"
				url="resourceCatalogManagement/v3/candidate1"
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
		var grid = this.shadowRoot.getElementById('candidateGrid');
		var ajaxGrid = this.shadowRoot.getElementById('getCandidateAjax');
		grid.dataProvider = this._getCandidate;
	}

	_getCandidate(params, callback) {
		var grid = this;
		var candidateList = document.body.querySelector('inventory-management').shadowRoot.querySelector('candidate-list').shadowRoot.getElementById('getCandidateAjax');
		if(candidateList.etag && params.page > 0) {
			headers['If-Range'] = candidateList.etag;
		}
		var candidateList1 = document.body.querySelector('inventory-management').shadowRoot.querySelector('candidate-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				candidateList1.etag = request.xhr.getResponseHeader('ETag');
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
			candidateList1.etag = null;
			var toast;
			toast.text = "error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
		callback([]);
		}
		if(candidateList.loading) {
			candidateList.lastRequest.completes.then(function(request) {
			var startRange = params.page * params.pageSize + 1;
			candidateList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (candidateList1.etag && params.page > 0) {
					candidateList.headers['If-Range'] = candidateList1.etag;
				} else {
					delete candidateList.headers['If-Range'];
				}
				return candidateList.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
			} else {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				candidateList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (candidateList1.etag && params.page > 0) {
					candidateList.headers['If-Range'] = candidateList1.etag;
				} else {
					delete candidateList.headers['If-Range'];
				}
			candidateList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('candidate-list', candidateList);
