/*
 * @license
 * Copyright 2018 - 2023 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js';

class httpList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="httpGrid"
					loading="{{loading}}">
				<vaadin-grid-column
								width="16ex"
								flex-grow="2"
								resizable="true">
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
						<div class="timestamp">
							<bdo dir="ltr">[[item.date]]</bdo>
						</div>
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="2"
						resizable="true">
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
						flex-grow="3"
						resizable="true">
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
						width="10ex"
						flex-grow="0">
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
						flex-grow="5"
						resizable="true">
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
					flex-grow="1"
					resizable="true">
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
			id="httpGetAjax"
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
		if(!grid.size) {
				grid.size = 0;
		}
		var httpList = document.body.querySelector('inventory-management').shadowRoot.querySelector('http-list');
		var ajax = httpList.shadowRoot.getElementById('httpGetAjax');
		var handleAjaxResponse = function(request) {
			if(request) {
				httpList.etag = request.xhr.getResponseHeader('ETag');
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
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			httpList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
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
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (httpList.etag && params.page > 0) {
					ajax.headers['If-Range'] = httpList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (httpList.etag && params.page > 0) {
				ajax.headers['If-Range'] = httpList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('http-list', httpList);
