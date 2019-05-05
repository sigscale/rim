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
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import './style-element.js';

class userListAjax extends PolymerElement {
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
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddUserModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getUserAjax"
				url="partyManagement/v2/individual"
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

//	showAddUserModal(event) {
//		document.getElementById("addUserModal").open();
//		document.body.querySelector('inventory-management').shadowRoot.getElementById('addUserModal').open();
//	}

	_getLog(params, callback) {
		var grid = this;
		var userListAjax = document.body.querySelector('inventory-management').shadowRoot.querySelector('user-list').shadowRoot.getElementById('getUserAjax');
		if(userListAjax.etag && params.page > 0) {
			headers['If-Range'] = userListAjax.etag;
		}
		var userListAjax = document.body.querySelector('inventory-management').shadowRoot.querySelector('user-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				userListAjax.etag = request.xhr.getResponseHeader('ETag');
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
			userListAjax.etag = null;
			var toast;
			toast.text = "error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(userListAjax.loading) {
			userListAjax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				userListAjax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userListAjax.etag && params.page > 0) {
					userListAjax.headers['If-Range'] = userListAjax.etag;
				} else {
					delete userListAjax.headers['If-Range'];
				}
				return userListAjax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			userListAjax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (userListAjax.etag && params.page > 0) {
				userListAjax.headers['If-Range'] = userListAjax.etag;
			} else {
				delete userListAjax.headers['If-Range'];
			}
			userListAjax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('user-list', userListAjax);
