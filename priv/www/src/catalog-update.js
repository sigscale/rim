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

class catalogUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="catalogUpdateModal" modal>
				<app-toolbar>
					<div main-title>Update Catalog</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							label="Id"
							value="{{catalogId}}"
							disabled>
					</paper-input>
					<paper-input
							label="Name"
							value="{{catalogName}}"
							required>
					</paper-input>
					<paper-textarea
							label="Description"
							value="{{catalogDescription}}">
					</paper-textarea>
					<paper-input
							label="Class"
							value="{{catalogType}}">
					</paper-input>
					<paper-dropdown-menu
						class="drop"
						label="Status"
						value="{{catalogStatus}}"
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
				id="catalogDeleteAjax"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
			<iron-ajax
				id="catalogUpdateAjax"
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
			catalogId: {
				type: String
			},
			catalogName: {
				type: String
			},
			catalogDescription: {
				type: String
			},
			catalogType: {
				type: String
			},
			catalogStatus: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.catalogId = item.id;
			this.catalogName = item.name;
			this.catalogDescription = item.description;
			this.catalogType = item.type;
			this.catalogStatus = item.status;
			this.$.catalogUpdateModal.open();
		} else {
			this.catalogId = null;
			this.catalogName = null;
			this.catalogDescription = null;
			this.catalogType = null;
			this.catalogStatus = null;
		}
	}

	_cancel() {
		this.$.catalogUpdateModal.close();
		this.catalogId = null;
		this.catalogName = null;
		this.catalogDescription = null;
		this.catalogType = null;
		this.catalogStatus = null;
	}

	_delete() {
		var ajax = this.$.catalogDeleteAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v4/resourceCatalog/" + this.catalogId;
		ajax.generateRequest();
	}

	_update() {
		var ajax = this.$.catalogUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v4/resourceCatalog/" + this.catalogId;
		var cat = new Object();
		if(this.catalogId) {
			cat.id= this.catalogId;
		}
		if(this.catalogName) {
			cat.name = this.catalogName;
		}
		if(this.catalogDescription) {
			cat.description = this.catalogDescription;
		}
		if(this.catalogType) {
			cat["@type"] = this.catalogType;
		}
		if(this.catalogStatus) {
			cat.lifecycleStatus = this.catalogStatus;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_response() {
		this.$.catalogUpdateModal.close();
		document.body.querySelector('inventory-management').shadowRoot.getElementById('catalogList').shadowRoot.getElementById('catalogGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-update', catalogUpdate);
