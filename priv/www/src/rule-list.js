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
import '@vaadin/vaadin-grid/vaadin-grid-sorter.js';
import './style-element.js';

class ruleList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="ruleGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column
						width="20ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="ruleId">
							<vaadin-grid-filter
									id="filter"
									aria-label="Id"
									path="ruleId"
									value="{{_filterRulesId}}">
								<input
										slot="filter"
										placeholder="Id"
										value="{{_filterRulesId::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="ruleDescription">
							<vaadin-grid-filter
									id="filter"
									aria-label="Description"
									path="ruleDescription"
									value="{{_filterRulesDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterRulesDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.description]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="rules">
							<vaadin-grid-filter
									id="filter"
									aria-label="Rules"
									path="rules"
									value="{{_filterRules}}">
								<input
										slot="filter"
										placeholder="Rules"
										value="{{_filterRules::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.rules]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax
				id="rulesGetAjax"
				url="resourceInventoryManagement/v4/logicalResource"
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
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterRulesId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterRulesDescription: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterRules: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.ruleGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.ruleGrid.selectedItems = [];
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('ruleGrid');
		grid.dataProvider = this._getRules;
	}

	_getRules(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var ruleList = document.body.querySelector('inventory-management').shadowRoot.querySelector('rule-list');
		var ajax = ruleList.shadowRoot.getElementById('rulesGetAjax');
		var handleAjaxResponse = function(request) {
			if(request) {
				ruleList.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.description = request.response[index].description;
					newRecord.rules = request.response[index].rule;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			ruleList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
         toast.text = error;
         toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (ruleList.etag && params.page > 0) {
					ajax.headers['If-Range'] = ruleList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (ruleList.etag && params.page > 0) {
				ajax.headers['If-Range'] = ruleList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('ruleGrid');
		grid.size = 0;
   }
}

window.customElements.define('rule-list', ruleList);
