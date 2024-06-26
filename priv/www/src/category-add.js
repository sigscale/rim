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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-input/paper-textarea.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class categoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="categoryAddModal" modal>
				<app-toolbar>
					<div main-title>Add Category</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{categoryName}}">
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{categoryDescription}}">
				</paper-textarea>
				<paper-input
						label="Version"
						value="{{categoryVersion}}">
				</paper-input>
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
							class="submit-button"
							on-tap="_add">
						Add
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="categoryAddAjax"
					content-type="application/json"
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
			categoryVersion: {
				type: String
			},
			categoryParent: {
				type: String
			},
			categoryRoot: {
				type: Boolean
			}
		}
	}

   ready() {
      super.ready()
	}

	_cancel() {
		this.$.categoryAddModal.close();
		this.categoryName = null;
		this.categoryDescription = null;
		this.categoryType = null;
		this.categoryStatus = null;
		this.categoryVersion = null;
		this.categoryParent = null;
		this.categoryRoot = false;
	}

	_add() {
		var ajax = this.$.categoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v4/resourceCategory/";
		var cat = new Object();
		if(this.categoryName) {
			cat.name = this.categoryName;
		}
		if(this.categoryDescription) {
			cat.desription = this.categoryDescription;
		}
		if(this.categoryType) {
			cat['@type'] =  this.categoryType;
		}
		if(this.categoryStatus) {
			cat.lifecycleStatus =  this.categoryStatus;
		}
		if(this.categoryVersion) {
			cat.version = this.categoryVersion;
		}
		if(this.categoryParent) {
			cat.parentId =  this.categoryParent;
		}
		if(this.categoryRoot) {
			cat.isRoot =  this.categoryRoot;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_response() {
		this.$.categoryAddModal.close();
		this.categoryName = null;
		this.categoryDescription = null;
		this.categoryType = null;
		this.categoryStatus = null;
		this.categoryVersion = null;
		this.categoryParent = null;
		this.categoryRoot = false;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('categoryList').shadowRoot.getElementById('categoryGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('category-add', categoryAdd);
