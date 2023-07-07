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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-input/paper-textarea.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import './style-element.js';

class categoryUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="categoryUpdateModal" modal>
				<app-toolbar>
					<div main-title>Update Category</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							label="Id"
							value="{{categoryId}}"
							disabled>
					</paper-input>
					<paper-input
							label="Name"
							value="{{categoryName}}"
							required>
					</paper-input>
					<paper-textarea
							label="Description"
							value="{{categoryDescription}}">
					</paper-textarea>
					<paper-input
							label="Class"
							value="{{categoryType}}">
					</paper-input>
					<paper-dropdown-menu
						class="drop"
						label="Status"
						value="{{categoryStatus}}"
						no-animations="true">
						<paper-listbox
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
							label="Parent"
							value="{{categoryParent}}">
					</paper-input>
					<paper-input
							label="Root"
							value="{{categoryRoot}}">
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
								on-tap="_cancel">
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
				id="categoryDeleteAjax"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
			<iron-ajax
				id="categoryUpdateAjax"
				content-type="application/merge-patch+json"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			categoryId: {
				type: String
			},
			categoryName: {
				type: String
			},
			categoryDescription: {
				type: String
			},
			categoryType: {
				type: String
			},
			categoryStatus: {
				type: String
			},
			categoryParent: {
				type: String
			},
			categoryRoot: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.categoryId = item.id;
			this.categoryName = item.name;
			this.categoryDescription = item.description;
			this.categoryType = item.type;
			this.categoryStatus = item.status;
			this.categoryParent = item.parent;
			this.categoryRoot = item.root;
			this.$.categoryUpdateModal.open();
		} else {
			this.categoryId = null;
			this.categoryName = null;
			this.categoryDescription = null;
			this.categoryType = null;
			this.categoryStatus = null;
			this.categoryParent = null;
			this.categoryRoot = null;
		}
	}

	_cancel() {
		this.$.categoryUpdateModal.close();
		this.categoryId = null;
		this.categoryName = null;
		this.categoryDescription = null;
		this.categoryType = null;
		this.categoryStatus = null;
		this.categoryParent = null;
		this.categoryRoot = null;
	}

	_delete() {
		var ajax = this.$.categoryDeleteAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v4/resourceCategory/" + this.$.categoryId.value;
		ajax.generateRequest();
		var deleteObj =  document.body.querySelector('inventory-management').shadowRoot.querySelector('category-update').shadowRoot.getElementById('categoryUpdateModal');
		deleteObj.close();
	}

	_update() {
		var ajax = this.$.categoryUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v4/resourceCategory/" + this.$.categoryId.value;
		var cat = new Object();
		if(this.categoryId) {
			cat.id = this.categoryId;
		}
		if(this.categoryName) {
			cat.name = this.categoryName;
		}
		if(this.categoryDescription) {
			cat.description = this.categoryDescription;
		}
		if(this.categoryType) {
			cat["@type"] = this.categoryType;
		}
		if(this.categoryStatus) {
			cat.lifecycleStatus = this.categoryStatus;
		}
		if(this.categoryParent) {
			cat.parentId = this.categoryParent;
		}
		if(this.categoryRoot) {
			cat.isRoot = this.categoryRoot;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_response() {
		this.$.categoryUpdateModal.close();
		document.body.querySelector('inventory-management').shadowRoot.getElementById('categoryList').shadowRoot.getElementById('categoryGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('category-update', categoryUpdate);
