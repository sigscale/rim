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
import './style-element.js';

class userList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="userGrid">
				<vaadin-grid-column>
					<template class="header">
						User name
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Language
					</template>
					<template>
						[[item.language]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax
				id="getUserAjax"
				url="partyManagement/v1/individual"
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
		var grid = this.shadowRoot.getElementById('userGrid');
		var ajaxGrid = this.shadowRoot.getElementById('getUserAjax');
		grid.dataProvider = this._getLog;
	}

	_getLog(params, callback) {
		var grid = this;
		var userList = document.body.querySelector('inventory-management').shadowRoot.querySelector('user-list').shadowRoot.getElementById('getUserAjax');
		if(userList.etag && params.page > 0) {
			headers['If-Range'] = userList.etag;
		}
		var userList1 = document.body.querySelector('inventory-management').shadowRoot.querySelector('user-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				userList1.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic){
					return characteristic.name == "locale";
				}
				for(var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					var langChar = request.response[index].characteristic.find(checkChar);
					if(langChar != undefined) {
						newRecord.language = langChar.value;
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
			userList1.etag = null;
			var toast;
			toast.text = "error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(userList.loading) {
			userList.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				userList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userList1.etag && params.page > 0) {
					userList.headers['If-Range'] = userList1.etag;
				} else {
					delete userList.headers['If-Range'];
				}
				return userList.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
			} else {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				userList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userList1.etag && params.page > 0) {
					userList.headers['If-Range'] = userList1.etag;
				} else {
					delete userList.headers['If-Range'];
				}
				userList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
			}
	}
}

window.customElements.define('user-list', userList);
