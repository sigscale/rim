/*
 * @license
 * Copyright 2018 - 2024 SigScale Global Inc.
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
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import './style-element.js';

class userList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="userGrid"
					loading="{{loading}}">
				<vaadin-grid-column
						width="28ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						User name
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="6ex"
						flex-grow="1"
						resizable="true">
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
			loading: {
				type: Boolean,
				notify: true
			},
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('userGrid');
		grid.dataProvider = this._getUsers;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.userGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.userGrid.selectedItems = [];
		}
	}

	_getUsers(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var userList = document.body.querySelector('inventory-management').shadowRoot.querySelector('user-list');
		var ajax = userList.shadowRoot.getElementById('getUserAjax');
		if(userList.etag && params.page > 0) {
			ajax.headers['If-Range'] = userList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				userList.etag = request.xhr.getResponseHeader('ETag');
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
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			userList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userList.etag && params.page > 0) {
					ajax.headers['If-Range'] = userList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (userList.etag && params.page > 0) {
				ajax.headers['If-Range'] = userList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('user-list', userList);
