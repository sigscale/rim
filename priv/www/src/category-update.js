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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class categoryUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateCategoryModal" modal>
				<app-toolbar>
					<div main-title>Update Category</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							id="categoryId"
							label="Id"
							value="{{category.categoryId}}"
							disabled>
					</paper-input>
					<paper-input
							id="categoryName"
							label="Name"
							value="{{category.categoryName}}"
							required>
					</paper-input>
					<paper-input
							id="categoryDesc"
							label="Description"
							value="{{category.categoryDescription}}">
					</paper-input>
					<paper-input
							id="categoryType"
							label="Class"
							value="{{category.categoryClass}}">
					</paper-input>
					<paper-dropdown-menu
						id="categoryStatus" 
						class="drop"
						label="Status"
						value="{{category.categoryStatus}}"
						no-animations="true">
						<paper-listbox
								id="updateStatus"
								slot="dropdown-content">
							<paper-item>In Study</paper-item>
							<paper-item>In Design</paper-item>
							<paper-item>In Test</paper-item>
							<paper-item>Rejected</paper-item>
							<paper-item>Active</paper-item>
							<paper-item>Launched</paper-item>
							<paper-item>Retired</paper-item>
							<paper-item>Obsolete</paper-item>
						</paper-listbox>
					</paper-dropdown-menu>
					<paper-input
							id="categoryParent"
							label="Parent"
							value="{{category.categoryParent}}">
					</paper-input>
					<paper-input
							id="categoryRoot"
							label="Root"
							value="{{category.categoryRoot}}">
					</paper-input>
					<div class="buttons">
						<paper-button
								raised
								class="update-button"
								on-tap="_update">
							Update
						</paper-button>
						<paper-button
								class="cancel-button"
								dialog-dismiss>
							Cancel
						</paper-button>
						<paper-button
								toggles
								raised
								class="delete-button"
								on-tap="_delete">
							Delete
						</paper-button>
					</div>
			</paper-dialog>
			<iron-ajax
				id="deleteCategoryAjax"
				loading="{{loading}}"
				on-response="_categoryUpdateResponse"
				on-error="_categoryUpdateError">
			</iron-ajax>
			<iron-ajax
				id="categoryUpdateAjax"
				content-type="application/merge-patch+json"
				loading="{{loading}}"
				on-response="_categoryUpdateResponse"
				on-error="_categoryUpdateError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			category: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_delete() {
		var ajax = this.$.deleteCategoryAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v3/resourceCategory/" + this.$.categoryId.value;
		ajax.generateRequest();
		var deleteObj =  document.body.querySelector('inventory-management').shadowRoot.querySelector('category-update').shadowRoot.getElementById('updateCategoryModal');
		deleteObj.close();
	}

	_update() {
		var ajax = this.$.categoryUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCategory/" + this.$.categoryId.value;
		var cat = new Object();
		if(this.$.categoryName.value) {
			cat.name = this.$.categoryName.value;
		}
		if(this.$.categoryDesc.value) {
			cat.description = this.$.categoryDesc.value;
		}
		if(this.$.categoryType) {
			cat["@type"] = this.$.categoryType.value;
		}
		if(this.$.categoryStatus.value) {
			cat.lifecycleStatus = this.$.categoryStatus.value;
		}
		if(this.$.categoryParent.value) {
			cat.parentId = this.$.categoryParent.value;
		}
		if(this.$.categoryRoot.value) {
			cat.isRoot = this.$.categoryRoot.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_categoryUpdateResponse() {
		var shell = document.body.querySelector('inventory-management').shadowRoot;
		shell.querySelector('category-update').shadowRoot.getElementById('updateCategoryModal').close();
		shell.getElementById('categoryList').shadowRoot.getElementById('categoryGrid').clearCache();
	}

	_categoryUpdateError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('category-update', categoryUpdateList);
