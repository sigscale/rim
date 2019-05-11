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

class specificationList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="specificationGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filterSpecName"
									aria-label="Name"
									path="name"
									value="{{_filterSpecName}}">
								<input
										slot="filter"
										placeholder="Name"
										value="{{_filterSpecName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.specName]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="20ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-sorter
								path="decription">
							<vaadin-grid-filter
									id="filterSpecDesc"
									aria-label="Description"
									path="description"
									value="{{_filterSpecDesc}}">
								<input
									slot="filter"
									placeholder="Description"
									value="{{_filterSpecDesc::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.specDesc]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filterSpecClass"
									aria-label="Class"
									path="@type"
									value="{{_filterSpecClass}}">
								<input
									slot="filter"
									placeholder="Class"
									value="{{_filterSpecClass::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.specClass]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="lifecycleStatus">
							<vaadin-grid-filter
									id="filterSpecStatus"
									aria-label="Status"
									path="lifecycleStatus"
									value="{{_filterSpecStatus}}">
								<input
									slot="filter"
									placeholder="Status"
									value="{{_filterSpecStatus::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.specStatus]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="category">
							<vaadin-grid-filter
									id="filterSpecCat"
									aria-label="Category"
									path="category"
									value="{{_filterSpecCategory}}">
								<input
									slot="filter"
									placeholder="Category"
									value="{{_filterSpecCategory::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.specCat]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="6ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-sorter
								path="isBundle">
							<vaadin-grid-filter
									id="filterSpecBundle"
									aria-label="Bundle"
									path="isBundle"
									value="{{_filterSpecBundle}}">
								<input
									slot="filter"
									placeholder="Bundle"
									value="{{_filterSpecBundle::input}}"
									focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.specBundle]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddSpecificationModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getSpecificationAjax"
				url="resourceCatalogManagement/v3/resourceSpecification"
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
			_filterSpecName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecDesc: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecClass: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecCategory: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSpecBundle: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activeItemChanged(item) {
		if(item) {
			var grid = this.$.specificationGrid;
			grid.selectedItems = item ? [item] : [];
			var updateSpec = document.querySelector('inventory-management').shadowRoot.getElementById('updateSpec');
			updateSpec.shadowRoot.getElementById('updateSpecModal').open();
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.dataProvider = this._getSpecification;
	}

	_getSpecification(params, callback) {
		var grid = this;
		var ajax = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list').shadowRoot.getElementById('getSpecificationAjax');
		var specificationList = document.body.querySelector('inventory-management').shadowRoot.querySelector('specification-list');
		delete ajax.params['filter'];
		delete ajax.params['sort'];
		var query = "";
		function checkLike(param) {
			return param.path == "name" || param.path == "description"
				|| param.path == "@type" || param.path == "category";
		}
		params.filters.filter(checkLike).forEach(function(filter) {
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
		function checkBool(param) {
			return param.path == "isBundle";
		}
		params.filters.filter(checkBool).forEach(function(filter) {
			if(filter.value) {
				if("true".startsWith(filter.value)) {
					if (query) {
						query = query + ",isBundle=true";
					} else {
						query = "[{isBundle=true";
					}
				} else if("false".startsWith(filter.value)) {
					if (query) {
						query = query + ",isBundle=false";
					} else {
						query = "[{isBundle=false";
					}
				} else {
					if (query) {
						query = query + ",isBundle=" + filter.value;
					} else {
						query = "[{isBundle=" + filter.value;
					}
				}
			}
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
		}
		if(specificationList.etag && params.page > 0) {
			ajax.headers['If-Range'] = specificationList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				specificationList.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.specName = request.response[index].name;
					newRecord.specDesc = request.response[index].description;
					newRecord.specClass = request.response[index]["@type"];
					newRecord.specStatus = request.response[index].lifecycleStatus;
					newRecord.specCat = request.response[index].category;
					newRecord.specBundle = request.response[index].isBundle;
					newRecord.specChars = request.response[index].resourceSpecCharacteristic;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			specificationList.etag = null;
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
					if (specificationList.etag && params.page > 0) {
						ajax.headers['If-Range'] = specificationList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (specificationList.etag && params.page > 0) {
				ajax.headers['If-Range'] = specificationList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('specificationGrid');
		grid.size = 0;
	}
}

window.customElements.define('specification-list', specificationList);
