/**
 * @license
 * Copyright (c) 2016 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@vaadin/vaadin-lumo-styles/color.js';
import '@polymer/polymer/lib/elements/dom-repeat.js';
import './style-element.js';

class httpList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="httpGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column
								width="28ex"
								flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
									aria-label="dateTime"
									path="datetime"
									value="[[_filterDateTime]]">
							<input
									slot="filter"
									placeholder="DateTime"
									value="{{_filterDateTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.date]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="4">
					<template class="header">
						<vaadin-grid-filter
								aria-label="host"
								path="host"
								value="[[_filterHost]]">
							<input
								slot="filter"
								placeholder="Host"
								value="{{_filterHost::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.host]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="8ex"
						flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="user"
								path="user"
								value="[[_filterUser]]">
							<input
									slot="filter"
									placeholder="User"
									value="{{_filterUser::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.user]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="8ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="method"
								path="method"
								value="[[_filterMethod]]">
							<input
									slot="filter"
									placeholder="Method"
									value="{{_filterMethod::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.method]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="10">
					<template class="header">
						<vaadin-grid-filter
								aria-label="resource"
								path="uri"
								value="[[_filterResource]]">
							<input
									slot="filter"
									placeholder="Resource"
									value="{{_filterResource::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.uri]]
					</template>
			</vaadin-grid-column>
			<vaadin-grid-column
					width="8ex"
					flex-grow="3">
				<template class="header">
					<vaadin-grid-filter
							aria-label="status"
							path="httpStatus"
							value="[[_filterStatus]]">
						<input
								slot="filter"
								placeholder="Status"
								value="{{_filterStatus::input}}"
								focus-target>
					</vaadin-grid-filter>
				</template>
				<template>
					[[item.httpStatus]]
				</template>
			</vaadin-grid-column>
		</vaadin-grid>
		<iron-ajax
			id="getHttpList"
			url="im/v1/log/http"
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
			activeItem: {
					type: Boolean,
					observer: '_activeItemChanged'
				}
			}
		}

	_activeItemChanged(item, last) {
		if (item ||last) {
			var grid = this.shadowRoot.getElementById('httpGrid');
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item
			}
			function checkExist(log) {
				return log.id == current.id;
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
		var grid = this.shadowRoot.getElementById('httpGrid');
		grid.dataProvider = this._getHttpLog;
	}

	_getHttpLog(params, callback) {
		var grid = this;
		var httpList = document.body.querySelector('inventory-management').shadowRoot.querySelector('http-list').shadowRoot.getElementById('getHttpList');
		var httpList1 = document.body.querySelector('inventory-management').shadowRoot.querySelector('http-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				httpList1.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.date = request.response[index].date;
					newRecord.host = request.response[index].host;
					newRecord.user = request.response[index].user;
					newRecord.method = request.response[index].method;
					newRecord.uri = request.response[index].uri;
					newRecord.httpStatus = request.response[index].httpStatus;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			httpList1.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
				callback([]);
		}
		if(httpList.loading) {
			httpList.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				httpList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (httpList1.etag && params.page > 0) {
					httpList.headers['If-Range'] = httpList1.etag;
				} else {
					delete httpList.headers['If-Range'];
				}
				return httpList.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			httpList.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (httpList1.etag && params.page > 0) {
				httpList.headers['If-Range'] = httpList1.etag;
			} else {
				delete httpList.headers['If-Range'];
			}
			httpList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

	window.customElements.define('http-list', httpList);
