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

class candidateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="candidateGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filter"
									aria-label="Name"
									path="name"
									value="{{_filterCatalogName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterCatalogName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="description">
							<vaadin-grid-filter
									id="filter"
									aria-label="Description"
									path="description"
									value="{{_filterCatalogDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterCatalogDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.description]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filter"
									aria-label="Class"
									path="@type"
									value="{{_filterCatalogClass}}">
								<input
										slot="filter"
										placeholder="Class"
										value="{{_filterCatalogClass::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.type]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="lifecycleStatus">
							<vaadin-grid-filter
									id="filter"
									aria-label="Status"
									path="lifecycleStatus"
									value="{{_filterCatalogStatus}}">
								<input
										slot="filter"
										placeholder="Status"
										value="{{_filterCatalogStatus::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.status]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddCandidateModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="candidateGetAjax"
				url="resourceCatalogManagement/v3/resourceCandidate"
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
			_filterCatalogName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCatalogDescription: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCatalogClass: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCatalogStatus: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('candidateGrid');
		grid.dataProvider = this._getCandidate;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.candidateGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.candidateGrid.selectedItems = [];
		}
   }

	_getCandidate(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var candidateList = document.body.querySelector('inventory-management').shadowRoot.querySelector('candidate-list');
		var ajax = candidateList.shadowRoot.getElementById('candidateGetAjax');
		var query = "";
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "name" || param.path == "description"
				|| param.path == "@type";
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if(filter.value) {
				if (query) {
					query = query + "," + filter.path + ".like=[" + filter.value + "%]";
				} else {
					query = "[{" + filter.path + ".like=[" + filter.value + "%]";
				}
			}
		});
		function checkLifeCycle(param) {
			return param.path == "lifecycleStatus";
		}
		params.filters.filter(checkLifeCycle).forEach(function(filter) {
			if(filter.value) {
				if("Obsolete".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=Obsolete";
					} else {
						query = "[{lifecycleStatus=Obsolete";
					}
				} else if("Launched".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=Launched";
					} else {
						query = "[{lifecycleStatus=Launched";
					}
				} else if("Active".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=Active";
					} else {
						query = "[{lifecycleStatus=Active";
					}
				} else if("In ".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus.in=[In Study,In Design, In Test]";
					} else {
						query = "[{lifecycleStatus.in=[In Study,In Design, In Test]";
					}
				} else if("In Study".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=In Study";
					} else {
						query = "[{lifecycleStatus=In Study";
					}
				} else if("In Design".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=In Design";
					} else {
						query = "[{lifecycleStatus=In Design";
					}
				} else if("Re".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus.in=[Rejected,Retired]";
					} else {
						query = "[{lifecycleStatus.in=[Rejected,Retired]";
					}
				} else if("Rejected".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=Rejected";
					} else {
						query = "[{lifecycleStatus=Rejected";
					}
				} else if("Retired".startsWith(filter.value)) {
					if (query) {
						query = query + ",lifecycleStatus=Retired";
					} else {
						query = "[{lifecycleStatus=Retired";
					}
				} else {
					if (query) {
						query = query + ",lifecycleStatus=" + filter.value;
					} else {
						query = "[{lifecycleStatus=" + filter.value;
					}
				}
			}
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
		}
		if(candidateList.etag && params.page > 0) {
			ajax.headers['If-Range'] = candidateList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				candidateList.etag = request.xhr.getResponseHeader('ETag');
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
						newRecord.name = request.response[index].name;
						newRecord.description = request.response[index].description;
						newRecord.type = request.response[index]["@type"];
						newRecord.status = request.response[index].lifecycleStatus;
						vaadinItems[index] = newRecord;
					}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			candidateList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (candidateList.etag && params.page > 0) {
						ajax.headers['If-Range'] = candidateList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (candidateList.etag && params.page > 0) {
				ajax.headers['If-Range'] = candidateList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('candidateGrid');
		grid.size = 0;
	}

	showAddCandidateModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('candidate-add').shadowRoot.getElementById('candidateAddModal').open();
	}
}

window.customElements.define('candidate-list', candidateList);
