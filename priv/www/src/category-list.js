/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
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

class categoryList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="categoryGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column
						width="20ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="name">
							<vaadin-grid-filter
									id="filterCategoryName""
									aria-label="Name"
									path="name"
									value="{{_filterCategoryName}}">
								<input
										slot="filter""
										placeholder="Name"
										value="{{_filterCategoryName::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>
						[[item.name]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="5"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="description">
							<vaadin-grid-filter
									id="filterCategoryDescription"
									aria-label="Description"
									path="description"
									value="{{_filterCategoryDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterCategoryDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="@type">
							<vaadin-grid-filter
									id="filterCategoryClass"
									aria-label="Class"
									path="@type"
									value="{{_filterCategoryClass}}">
								<input
										slot="filter"
										placeholder="Class"
										value="{{_filterCategoryClass::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.type]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="lifecycleStatus">
							<vaadin-grid-filter
									id="filterCategoryStatus"
									aria-label="Status"
									path="lifecycleStatus"
									value="{{_filterCategoryStatus}}">
								<input
										slot="filter"
										placeholder="Status"
										value="{{_filterCategoryStatus::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.status]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="parentId">
							<vaadin-grid-filter
									id="filterCategoryParent"
									aria-label="Parent"
									path="parentId"
									value="{{_filterCategoryParent}}">
								<input
										slot="filter"
										placeholder="Parent"
										value="{{_filterCategoryParent::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.parent]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="11ex"
						flex-grow="1"
						resizable="true">
					<template class="header">
						<vaadin-grid-sorter
								path="isRoot">
							<vaadin-grid-filter
									id="filterCategoryRoot"
									aria-label="Root"
									path="isRoot"
									value="{{_filterCategoryRoot}}">
								<input
										slot="filter"
										placeholder="Root"
										value="{{_filterCategoryRoot::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.root]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddCategoryModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="categoryGetAjax"
				url="resourceCatalogManagement/v4/resourceCategory"
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
			_filterCategoryName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategoryDesc: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategoryClass: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategoryStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategoryParent: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCategoryRoot: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.categoryGrid.selectedItems = item ? [item] : [];
      } else {
			this.$.categoryGrid.selectedItems = [];
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('categoryGrid');
		grid.dataProvider = this._getCategory;
	}


	_getCategory(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var categoryList = document.body.querySelector('inventory-management').shadowRoot.querySelector('category-list');
		var ajax = categoryList.shadowRoot.getElementById('categoryGetAjax');
		var query = "";
		delete ajax.params['filter'];
		params.filters.forEach(function(filter) {
			if(filter.path != "lifecycleStatus") {
				if(filter.value) {
					if (query) {
						query = query + "," + filter.path + ".like=[" + filter.value + "%]";
					} else {
						query = "[{" + filter.path + ".like=[" + filter.value + "%]";
					}
				}
			} else if(filter.path == "isRoot") {
				if(filter.value) {
					if("true".startsWith(filter.value)) {
						if (query) {
							query = query + ",isRoot=true";
						} else {
							query = "[{isRoot=true";
						}
					} else if("false".startsWith(filter.value)) {
						if (query) {
							query = query + ",isRoot=false";
						} else {
							query = "[{isRoot=false";
						}
					} else {
						if (query) {
							query = query + ",isRoot=" + filter.value;
						} else {
							query = "[{isRoot=" + filter.value;
						}
					}
				}
			} else {
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
			}
		});
		if(query) {
			ajax.params['filter'] = "\"" + query + "}]\"";
		}
		if(categoryList.etag && params.page > 0) {
			ajax.headers['If-Range'] = categoryList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				categoryList.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.parent = request.response[index].parentId;
					newRecord.root = request.response[index].isRoot;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			categoryList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (categoryList.etag && params.page > 0) {
					ajax.headers['If-Range'] = categoryList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (categoryList.etag && params.page > 0) {
				ajax.headers['If-Range'] = categoryList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('categoryGrid');
		grid.size = 0;
	}

	showAddCategoryModal(event) {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('category-add').shadowRoot.getElementById('categoryAddModal').open();
	}
}

window.customElements.define('category-list', categoryList);
