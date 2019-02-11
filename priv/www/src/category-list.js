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

class categoryList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="categoryGrid">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryName">
							<vaadin-grid-filter
									id="filterCategoryName""
									aria-label="Name"
									path="categoryName"
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
						[[item.categoryName]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryDescription">
							<vaadin-grid-filter
									id="filterCategoryDescription"
									aria-label="Description"
									path="categoryDescription"
									value="{{_filterCategoryDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterCategoryDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.categoryDescription]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryClass">
							<vaadin-grid-filter
									id="filterCategoryClass"
									aria-label="Class"
									path="categoryClass"
									value="{{_filterCategoryClass}}">
								<input
										slot="filter"
										placeholder="Class"
										value="{{_filterCategoryClass::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.categoryClass]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryStatus">
							<vaadin-grid-filter
									id="filterCategoryStatus"
									aria-label="Status"
									path="categoryStatus"
									value="{{_filterCategoryStatus}}">
								<input
										slot="filter"
										placeholder="Status"
										value="{{_filterCategoryStatus::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.categoryStatus]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryParent">
							<vaadin-grid-filter
									id="filterCategoryParent"
									aria-label="Parent"
									path="categoryParent"
									value="{{_filterCategoryParent}}">
								<input
										slot="filter"
										placeholder="Parent"
										value="{{_filterCategoryParent::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.categoryParent]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="categoryRoot">
							<vaadin-grid-filter
									id="filterCategoryRoot"
									aria-label="Root"
									path="categoryRoot"
									value="{{_filterCategoryRoot}}">
								<input
										slot="filter"
										placeholder="Root"
										value="{{_filterCategoryRoot::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
					<template>[[item.categoryRoot]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddCategoryModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getCategoryAjax"
				url="resourceCatalogManagement/v3/category"
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
		var grid = this.shadowRoot.getElementById('categoryGrid');
		var ajaxGrid = this.shadowRoot.getElementById('getCategoryAjax');
		grid.dataProvider = this._getCategory;
	}

	_getCategory(params, callback) {
		var grid = this;
		var categoryList = document.body.querySelector('inventory-management').shadowRoot.querySelector('category-list').shadowRoot.getElementById('getCategoryAjax');
		var query = "";
		function checkHead(param) {
         return param.path == "categoryName" || param.path == "categoryDescription"
            || param.path == "categoryClass" || param.path == "categoryStatus"
				|| param.path == "categoryParent" || param.path == "categoryRoot";
      }
      params.filters.filter(checkHead).forEach(function(filter) {
         if(filter.value) {
            if (query) {
               query = query + "]," + filter.path + ".like=[" + filter.value + "%";
            } else {
               query = "[{" + filter.path + ".like=[" + filter.value + "%";
            }
         }
      });
      if(query) {
         if(query.includes("like=[%")) {
            delete params.filters[0];
            categoryList.params['filter'] = "resourceCatalogManagement/v3/category";
         } else {
            categoryList.params['filter'] = "\"" + query + "]}]\"";
         }
      }
		if(categoryList.etag && params.page > 0) {
			headers['If-Range'] = categoryList.etag;
		}
		var categoryList1 = document.body.querySelector('inventory-management').shadowRoot.querySelector('category-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				categoryList1.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.categoryName = request.response[index].name;
					newRecord.categoryDescription = request.response[index].description;
					newRecord.categoryClass = request.response[index]["@baseType"];
					newRecord.categoryStatus = request.response[index].lifecycleStatus;
					newRecord.categoryParent = request.response[index].parentId;
					newRecord.categoryRoot = request.response[index].isRoot;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			categoryList1.etag = null;
			var toast;
			toast.text = "error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
		callback([]);
		}
		if(categoryList.loading) {
			categoryList.lastRequest.completes.then(function(request) {
			var startRange = params.page * params.pageSize + 1;
			categoryList.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (categoryList1.etag && params.page > 0) {
				categoryList.headers['If-Range'] = userList1.etag;
			} else {
				delete categoryList.headers['If-Range'];
			}
				return categoryList.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
			} else {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				categoryList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (categoryList1.etag && params.page > 0) {
					categoryList.headers['If-Range'] = userList1.etag;
				} else {
					delete categoryList.headers['If-Range'];
				}
			categoryList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('category-list', categoryList);
